#lang racket

;; Query IMAP QUOTA for each account to see storage usage.
;;
;; Usage:
;;   racket quota-report.rkt
;;
;; Uses the IMAP GETQUOTAROOT command (RFC 2087) to report storage
;; usage for each configured account. Not all IMAP servers support
;; QUOTA — those that don't will show "not supported".
;;
;; Note: Gmail's quota covers Drive + Gmail + Photos combined,
;; so the usage shown may exceed what mail alone accounts for.

(require openssl
         net/imap
         net/base64
         net/url
         net/uri-codec
         json
         "src/imap-email-account-credentials.rkt"
         "src/gmail-oauth2.rkt"
         "src/oauth2-details.rkt"
         "src/utils.rkt")

(handle-broken-pipe)

;; ---- credentials ----

(define (load-credentials)
  (read-email-account-credentials-hash-from-file-named
   (default-credentials-filepath)))

;; ---- raw IMAP session for QUOTA ----
;; We can't use net/imap's high-level API for QUOTA since it doesn't
;; expose the command. Instead, we do a raw SSL+IMAP session.

(define tag-counter 0)

(define (next-tag)
  (set! tag-counter (add1 tag-counter))
  (format "A~a" tag-counter))

(define (send-command out cmd)
  (let ([tag (next-tag)])
    (fprintf out "~a ~a\r\n" tag cmd)
    (flush-output out)
    tag))

(define (read-response in tag)
  (let loop ([lines '()])
    (let ([line (read-line in 'return-linefeed)])
      (cond
        [(eof-object? line) (reverse lines)]
        [(string-prefix? line (string-append tag " "))
         (reverse (cons line lines))]
        [else (loop (cons line lines))]))))

;; Parse a QUOTA response line like:
;;   * QUOTA "" (STORAGE 1234567 15728640)
;; Returns (list used-kb limit-kb) or #f
(define quota-rx #px"\\* QUOTA [^ ]+ \\(STORAGE (\\d+) (\\d+)\\)")

(define (parse-quota-response lines)
  (for/or ([line lines])
    (let ([m (regexp-match quota-rx line)])
      (and m
           (list (string->number (second m))
                 (string->number (third m)))))))

;; Build XOAUTH2 auth string for Gmail
(define (xoauth2-auth-string email access-token)
  (let ([raw (format "user=~a\x01auth=Bearer ~a\x01\x01" email access-token)])
    (bytes->string/latin-1 (base64-encode (string->bytes/utf-8 raw) #""))))

(define (query-quota-raw credential)
  (let ([email (imap-email-account-credentials-mailaddress credential)]
        [hostname (imap-email-account-credentials-hostname credential)]
        [port-no 993])
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "  ERROR: ~a~n" (exn-message e))
                                 #f)])
      (let-values ([(in out) (ssl-connect hostname port-no)])
        ;; Read server greeting
        (read-line in 'return-linefeed)

        ;; Authenticate
        (if (imap-email-account-credentials-xoauth2? credential)
            ;; OAuth2
            (let* ([oauth2-creds (load-google-oauth2-details)]
                   [tokens (get-valid-access-token email oauth2-creds)]
                   [access-token (hash-ref tokens 'access_token)]
                   [auth-str (xoauth2-auth-string email access-token)]
                   [tag (send-command out (format "AUTHENTICATE XOAUTH2 ~a" auth-str))]
                   [resp (read-response in tag)])
              (unless (ormap (lambda (l) (regexp-match? #rx"^A[0-9]+ OK" l)) resp)
                (error 'query-quota-raw "Auth failed for ~a" email)))
            ;; Password
            (let* ([password (imap-email-account-credentials-password credential)]
                   [tag (send-command out (format "LOGIN \"~a\" \"~a\"" email password))]
                   [resp (read-response in tag)])
              (unless (ormap (lambda (l) (regexp-match? #rx"^A[0-9]+ OK" l)) resp)
                (error 'query-quota-raw "Login failed for ~a" email))))

        ;; Send GETQUOTAROOT for INBOX
        (let* ([tag (send-command out "GETQUOTAROOT INBOX")]
               [resp (read-response in tag)])

          ;; Logout
          (let ([ltag (send-command out "LOGOUT")])
            (with-handlers ([exn:fail? void])
              (read-response in ltag)))
          (close-input-port in)
          (close-output-port out)

          ;; Check if quota is supported
          (if (ormap (lambda (l) (regexp-match? #rx"^A[0-9]+ OK" l)) resp)
              (parse-quota-response resp)
              (begin
                ;; Check for NO response (not supported)
                (when (ormap (lambda (l) (regexp-match? #rx"^A[0-9]+ NO" l)) resp)
                  (printf "  QUOTA not supported by server~n"))
                #f)))))))

;; need this for oauth2
(define (get-valid-access-token email oauth2-creds)
  ;; Reuse the logic from gmail-oauth2.rkt by loading tokens
  (let* ([tokens-path (build-path (find-system-path 'home-dir)
                                   ".imap_secrets"
                                   (format ".oauth2_tokens_~a" email))]
         [saved (if (file-exists? tokens-path)
                    (call-with-input-file tokens-path read)
                    #f)])
    (cond
      [(and (hash? saved)
            (> (hash-ref saved 'expires_at 0) (+ (current-seconds) 60)))
       saved]
      [(and (hash? saved) (hash-ref saved 'refresh_token #f))
       ;; Refresh
       (let* ([client-id (oauth2-details-client-id oauth2-creds)]
              [client-secret (oauth2-details-client-secret oauth2-creds)]
              [post-data
               (alist->form-urlencoded
                (list (cons 'client_id client-id)
                      (cons 'client_secret client-secret)
                      (cons 'refresh_token (hash-ref saved 'refresh_token))
                      (cons 'grant_type "refresh_token")))]
              [response
               (post-pure-port
                (string->url "https://oauth2.googleapis.com/token")
                (string->bytes/utf-8 post-data)
                (list "Content-Type: application/x-www-form-urlencoded"))]
              [json-response (read-json response)])
         (if (hash-has-key? json-response 'access_token)
             (let ([tokens (hash 'access_token (hash-ref json-response 'access_token)
                                 'refresh_token (hash-ref saved 'refresh_token)
                                 'expires_at (+ (current-seconds)
                                                (hash-ref json-response 'expires_in 3600)))])
               (call-with-output-file tokens-path
                 (lambda (out) (write tokens out))
                 #:exists 'replace #:permissions #o600)
               tokens)
             (error 'get-valid-access-token "Token refresh failed for ~a" email)))]
      [else
       (error 'get-valid-access-token "No valid tokens for ~a. Run a fetch first." email)])))


;; ---- formatting ----

(define (format-pct used limit)
  (if (= limit 0) "N/A" (format "~a%" (~r (* 100.0 (/ used limit)) #:precision '(= 1)))))

;; ---- main ----

(define (main)
  (let ([creds (load-credentials)])
    (printf "~nIMAP Storage Quota Report~n")
    (printf "=========================~n~n")

    (printf "  ~a  ~a  ~a  ~a  ~a~n"
            (~a "Account" #:min-width 30)
            (~a "Used" #:min-width 12 #:align 'right)
            (~a "Limit" #:min-width 12 #:align 'right)
            (~a "%" #:min-width 7 #:align 'right)
            "Note")
    (printf "  ~a  ~a  ~a  ~a  ~a~n"
            (make-string 30 #\-)
            (make-string 12 #\-)
            (make-string 12 #\-)
            (make-string 7 #\-)
            (make-string 20 #\-))

    (for ([name (sort (hash-keys creds) string<?)])
      (let* ([credential (hash-ref creds name)]
             [email (imap-email-account-credentials-mailaddress credential)])
        (printf "  ~a" (~a email #:min-width 30))
        (flush-output)
        (let ([result (query-quota-raw credential)])
          (if result
              (let ([used-kb (first result)]
                    [limit-kb (second result)])
                (printf "  ~a  ~a  ~a  ~a~n"
                        (~a (format-size-kb used-kb) #:min-width 12 #:align 'right)
                        (~a (format-size-kb limit-kb) #:min-width 12 #:align 'right)
                        (~a (format-pct used-kb limit-kb) #:min-width 7 #:align 'right)
                        (cond
                          [(>= (/ used-kb limit-kb 1.0) 0.9) "⚠ nearly full"]
                          [(>= (/ used-kb limit-kb 1.0) 0.75) "getting full"]
                          [else ""])))
              (printf "  ~a  ~a  ~a  ~a~n"
                      (~a "-" #:min-width 12 #:align 'right)
                      (~a "-" #:min-width 12 #:align 'right)
                      (~a "-" #:min-width 7 #:align 'right)
                      "quota not available")))))

    (printf "~nNote: Gmail quotas include Drive + Gmail + Photos.~n")))

(main)
