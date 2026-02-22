#lang racket

;; Fetch RFC822.SIZE for all messages and update existing digests.
;;
;; Usage:
;;   racket fetch-sizes.rkt
;;
;; This connects to each IMAP account, fetches UID and RFC822.SIZE
;; for all messages, then patches the sizes into the latest digest
;; files. This is a non-destructive update — it reads the existing
;; digests and writes updated versions with message-size populated.
;;
;; This uses raw IMAP commands because Racket's net/imap doesn't
;; support the RFC822.SIZE fetch attribute.

(require openssl
         net/imap
         net/url
         net/uri-codec
         net/base64
         json
         racket/serialize
         gregor
         "src/imap-email-account-credentials.rkt"
         "src/connect-to-imap-account.rkt"
         "src/gmail-oauth2.rkt"
         "src/oauth2-details.rkt"
         "src/mailbox-digest.rkt"
         "src/main-mail-header-parts.rkt")

;; ---- credentials ----

(define (default-credentials-filepath)
  (build-path (find-system-path 'home-dir) ".imap_secrets" "credentials"))

(define (load-credentials)
  (read-email-account-credentials-hash-from-file-named
   (default-credentials-filepath)))

;; ---- raw IMAP helpers ----

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

;; Parse FETCH responses for UID and RFC822.SIZE
;; Lines look like: * 1 FETCH (UID 12345 RFC822.SIZE 4567)
(define fetch-size-rx #px"\\* \\d+ FETCH \\(UID (\\d+) RFC822\\.SIZE (\\d+)\\)")
(define fetch-size-rx2 #px"\\* \\d+ FETCH \\(RFC822\\.SIZE (\\d+) UID (\\d+)\\)")

(define (parse-size-responses lines)
  (let ([result (make-hash)])
    (for ([line lines])
      (let ([m (or (regexp-match fetch-size-rx line)
                   (regexp-match fetch-size-rx2 line))])
        (when m
          (let ([uid (string->number (if (regexp-match fetch-size-rx line)
                                          (second m)
                                          (third m)))]
                [size (string->number (if (regexp-match fetch-size-rx line)
                                           (third m)
                                           (second m)))])
            (hash-set! result uid size)))))
    result))

;; Build XOAUTH2 auth string
(define (xoauth2-auth-string email access-token)
  (let ([raw (format "user=~a\x01auth=Bearer ~a\x01\x01" email access-token)])
    (bytes->string/latin-1 (base64-encode (string->bytes/utf-8 raw) #""))))

;; Token refresh (same as quota-report)
(define (get-valid-access-token email oauth2-creds)
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

;; ---- raw IMAP session to fetch sizes ----

(define (fetch-sizes-raw credential folder-name msg-count)
  (let ([email (imap-email-account-credentials-mailaddress credential)]
        [hostname (imap-email-account-credentials-hostname credential)]
        [port-no 993])
    (let-values ([(in out) (ssl-connect hostname port-no)])
      ;; Read greeting
      (read-line in 'return-linefeed)

      ;; Authenticate
      (if (imap-email-account-credentials-xoauth2? credential)
          (let* ([oauth2-creds (load-google-oauth2-details)]
                 [tokens (get-valid-access-token email oauth2-creds)]
                 [access-token (hash-ref tokens 'access_token)]
                 [auth-str (xoauth2-auth-string email access-token)]
                 [tag (send-command out (format "AUTHENTICATE XOAUTH2 ~a" auth-str))]
                 [resp (read-response in tag)])
            (unless (ormap (lambda (l) (regexp-match? #rx"^A[0-9]+ OK" l)) resp)
              (error 'fetch-sizes-raw "Auth failed for ~a" email)))
          (let* ([password (imap-email-account-credentials-password credential)]
                 [tag (send-command out (format "LOGIN \"~a\" \"~a\"" email password))]
                 [resp (read-response in tag)])
            (unless (ormap (lambda (l) (regexp-match? #rx"^A[0-9]+ OK" l)) resp)
              (error 'fetch-sizes-raw "Login failed for ~a" email))))

      ;; SELECT folder
      (let* ([tag (send-command out (format "SELECT \"~a\"" folder-name))]
             [resp (read-response in tag)])
        (unless (ormap (lambda (l) (regexp-match? #rx"^A[0-9]+ OK" l)) resp)
          (error 'fetch-sizes-raw "Could not select ~a" folder-name)))

      ;; Fetch UID and RFC822.SIZE in batches
      (let ([uid-sizes (make-hash)]
            [batch-size 500])
        (let loop ([start 1])
          (when (<= start msg-count)
            (let* ([end (min msg-count (+ start batch-size -1))]
                   [tag (send-command out (format "FETCH ~a:~a (UID RFC822.SIZE)" start end))]
                   [resp (read-response in tag)]
                   [batch-sizes (parse-size-responses resp)])
              (for ([(uid size) (in-hash batch-sizes)])
                (hash-set! uid-sizes uid size))
              (when (= (modulo start 5000) 1)
                (printf "    ...fetched sizes for ~a of ~a~n" (min end msg-count) msg-count))
              (loop (+ end 1)))))

        ;; Logout
        (let ([tag (send-command out "LOGOUT")])
          (with-handlers ([exn:fail? void])
            (read-response in tag)))
        (close-input-port in)
        (close-output-port out)

        uid-sizes))))

;; ---- patch sizes into digest ----

(define (patch-digest-with-sizes digest-path uid-sizes)
  (let* ([mbd (load-mailbox-digest-from-file digest-path)]
         [patched 0]
         [updated-headers
          (for/list ([hdr (mailbox-digest-mail-headers mbd)])
            (let* ([uid (main-mail-header-parts-mail-id hdr)]
                   [size (hash-ref uid-sizes uid #f)])
              (if size
                  (begin
                    (set! patched (add1 patched))
                    (main-mail-header-parts
                     uid
                     (main-mail-header-parts-date-string hdr)
                     (main-mail-header-parts-from hdr)
                     (main-mail-header-parts-to hdr)
                     (main-mail-header-parts-cc hdr)
                     (main-mail-header-parts-bcc hdr)
                     (main-mail-header-parts-subj hdr)
                     (main-mail-header-parts-flags hdr)
                     (main-mail-header-parts-parsed-year hdr)
                     (main-mail-header-parts-parsed-epoch hdr)
                     size))
                  hdr)))]
         [updated-digest
          (mailbox-digest
           (mailbox-digest-mail-address mbd)
           (mailbox-digest-folder-name mbd)
           (mailbox-digest-uid-validity mbd)
           (mailbox-digest-index-range mbd)
           updated-headers
           (mailbox-digest-timestamp mbd))])
    (call-with-output-file digest-path
      (lambda (out) (write (serialize updated-digest) out))
      #:exists 'replace)
    (printf "    Patched ~a / ~a messages with sizes~n"
            patched (length updated-headers))))

;; ---- main ----

(define (main)
  (let ([creds (load-credentials)]
        [digest-dir (default-digest-dir)])

    (printf "Fetching message sizes and patching digests...~n~n")

    (for ([name (sort (hash-keys creds) string<?)])
      (let* ([credential (hash-ref creds name)]
             [email (imap-email-account-credentials-mailaddress credential)])

        ;; Find all digest files for this account
        (let ([digest-files
               (for/list ([f (directory-list digest-dir #:build? #t)]
                          #:when (and (regexp-match? #rx"\\.ser$" (path->string f))
                                      (string-contains? (path->string f) email)))
                 f)])

          ;; Group by folder, keep only latest per folder
          (let ([by-folder (make-hash)])
            (for ([f digest-files])
              (with-handlers ([exn:fail? (lambda (e) (void))])
                (let* ([mbd (load-mailbox-digest-from-file f)]
                       [folder (mailbox-digest-folder-name mbd)])
                  (let ([existing (hash-ref by-folder folder #f)])
                    (when (or (not existing)
                              (datetime>? (mailbox-digest-timestamp mbd)
                                          (mailbox-digest-timestamp (cdr existing))))
                      (hash-set! by-folder folder (cons f mbd)))))))

            (for ([folder (sort (hash-keys by-folder) string<?)])
              (let* ([pair (hash-ref by-folder folder)]
                     [digest-path (car pair)]
                     [mbd (cdr pair)]
                     [msg-count (mailbox-digest-count mbd)])

                (printf "  ~a / ~a (~a messages)~n" email folder msg-count)

                ;; Check if sizes are already populated
                (let ([already-has-sizes
                       (for/sum ([hdr (mailbox-digest-mail-headers mbd)])
                         (if (main-mail-header-parts-message-size hdr) 1 0))])
                  (if (= already-has-sizes msg-count)
                      (printf "    Already has sizes, skipping.~n")
                      (with-handlers
                          ([exn:fail?
                            (lambda (e)
                              (printf "    ERROR: ~a~n" (exn-message e)))])
                        (printf "    Connecting to fetch sizes...~n")
                        ;; We need live message count from server
                        (let ([imap-conn
                               (if (imap-email-account-credentials-xoauth2? credential)
                                   (let ([oauth2-creds (load-google-oauth2-details)])
                                     (oauth2-connect-to-imap email oauth2-creds folder))
                                   (securely-connect-to-imap-account credential folder))])
                          (let ([server-count (imap-messages imap-conn)])
                            (imap-disconnect imap-conn)
                            ;; Now fetch sizes via raw IMAP
                            (let ([uid-sizes (fetch-sizes-raw credential folder server-count)])
                              (printf "    Got sizes for ~a UIDs~n" (hash-count uid-sizes))
                              (patch-digest-with-sizes digest-path uid-sizes))))))))))))))

  (printf "~nDone.~n"))

(main)
