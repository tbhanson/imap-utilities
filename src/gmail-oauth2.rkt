#lang racket

;; OAuth2 authentication for Gmail IMAP.
;;
;; Flow:
;; 1. Check for saved tokens (~/.imap_secrets/.oauth2_tokens_<address>)
;; 2. If valid access token exists, use it
;; 3. If expired but refresh token exists, refresh it
;; 4. Otherwise, run the browser-based authorization flow:
;;    - Start a local web server on port 8080
;;    - Open the user's browser to Google's consent page
;;    - Catch the redirect with the authorization code
;;    - Exchange it for access + refresh tokens
;;    - Save tokens for next time

(require net/imap
         net/url
         net/uri-codec
         net/head
         openssl
         json
         racket/port
         racket/tcp
         "oauth2-details.rkt")

(provide
 (contract-out
  [load-google-oauth2-details (-> oauth2-details?)]
  [oauth2-connect-to-imap (-> string? oauth2-details? string? any/c)]
  ))

;; ---- token storage ----

;; Tokens are stored as a JSON-like hash:
;;   (hash 'access_token "..." 'refresh_token "..." 'expires_at <epoch-seconds>)

(define (tokens-filepath email-address)
  (build-path (find-system-path 'home-dir)
              ".imap_secrets"
              (format ".oauth2_tokens_~a" email-address)))

(define (save-tokens! email-address tokens)
  (let ([path (tokens-filepath email-address)])
    (call-with-output-file path
      (lambda (out) (write tokens out))
      #:exists 'replace
      #:permissions #o600)))

(define (load-tokens email-address)
  (let ([path (tokens-filepath email-address)])
    (if (file-exists? path)
        (call-with-input-file path read)
        #f)))

;; ---- Google OAuth2 details ----

(define (default-google-oauth2-filepath)
  (build-path (find-system-path 'home-dir) ".imap_secrets" ".oauth2_google"))

(define (load-google-oauth2-details)
  (let ([path (default-google-oauth2-filepath)])
    (unless (file-exists? path)
      (error 'load-google-oauth2-details
             "No Google OAuth2 credentials found at ~a\nSee README for setup instructions."
             path))
    (read-one-oauth2-details-from-file-named path)))

;; ---- authorization flow ----

(define GOOGLE-AUTH-URL "https://accounts.google.com/o/oauth2/v2/auth")
(define GOOGLE-TOKEN-URL "https://oauth2.googleapis.com/token")
(define GMAIL-SCOPE "https://mail.google.com/")

(define (build-authorization-url client-id redirect-uri)
  (format "~a?~a"
          GOOGLE-AUTH-URL
          (alist->form-urlencoded
           (list (cons 'client_id client-id)
                 (cons 'redirect_uri redirect-uri)
                 (cons 'response_type "code")
                 (cons 'scope GMAIL-SCOPE)
                 (cons 'access_type "offline")
                 (cons 'prompt "consent")))))

;; Start a temporary local web server, open the browser, and wait for
;; Google's redirect with the authorization code.
;; Uses a simple TCP listener instead of serve/servlet so we can
;; cleanly shut down the port between auth attempts.
(define (get-authorization-code client-id redirect-uri)
  (let ([auth-code-channel (make-channel)]
        [port-no (let ([m (regexp-match #rx":([0-9]+)" redirect-uri)])
                   (if m (string->number (second m)) 8080))])

    ;; Create a TCP listener we can close cleanly
    (let ([listener (tcp-listen port-no 4 #t "127.0.0.1")])

      ;; Handle one request in a background thread
      (let ([server-thread
             (thread
              (lambda ()
                (let-values ([(in out) (tcp-accept listener)])
                  ;; Read the HTTP request to extract the code
                  (let ([request-line (read-line in)])
                    ;; Parse ?code=... from the GET request
                    (let ([match (regexp-match #rx"[?&]code=([^& ]+)" request-line)])
                      (when match
                        (channel-put auth-code-channel (second match))))

                    ;; Send a minimal HTTP response
                    (display "HTTP/1.1 200 OK\r\n" out)
                    (display "Content-Type: text/html\r\n" out)
                    (display "Connection: close\r\n\r\n" out)
                    (display "<html><body><h1>Authorization Complete</h1>" out)
                    (display "<p>You can close this browser tab and return to your terminal.</p>" out)
                    (display "</body></html>" out)
                    (flush-output out)
                    (close-input-port in)
                    (close-output-port out)))))])

        ;; Open the user's browser
        (let ([auth-url (build-authorization-url client-id redirect-uri)])
          (printf "~nOpening your browser for Google authorization...~n")
          (printf "If it doesn't open automatically, visit this URL:~n~a~n~n" auth-url)
          (with-handlers ([exn:fail? (lambda (e)
                                       (printf "(Could not open browser automatically.)~n"))])
            (cond
              [(eq? (system-type) 'macosx)
               (system (format "open \"~a\"" auth-url))]
              [(eq? (system-type) 'unix)
               (system (format "xdg-open \"~a\"" auth-url))]
              [(eq? (system-type) 'windows)
               (system (format "start \"\" \"~a\"" auth-url))])))

        ;; Wait for the code (with timeout)
        (printf "Waiting for authorization (will timeout in 120 seconds)...~n")
        (let ([result (sync/timeout 120 auth-code-channel)])
          ;; Clean up: close the listener so the port is freed
          (tcp-close listener)
          (kill-thread server-thread)
          (if result
              (begin
                (printf "Authorization code received!~n")
                result)
              (error 'get-authorization-code
                     "Timed out waiting for authorization. Please try again.")))))))

;; ---- token exchange and refresh ----

(define (exchange-code-for-tokens auth-code client-id client-secret redirect-uri)
  (let* ([post-data
          (alist->form-urlencoded
           (list (cons 'client_id client-id)
                 (cons 'client_secret client-secret)
                 (cons 'code auth-code)
                 (cons 'grant_type "authorization_code")
                 (cons 'redirect_uri redirect-uri)))]
         [response
          (post-pure-port
           (string->url GOOGLE-TOKEN-URL)
           (string->bytes/utf-8 post-data)
           (list "Content-Type: application/x-www-form-urlencoded"))])
    (let ([json-response (read-json response)])
      (if (hash-has-key? json-response 'access_token)
          (hash 'access_token (hash-ref json-response 'access_token)
                'refresh_token (hash-ref json-response 'refresh_token #f)
                'expires_at (+ (current-seconds)
                               (hash-ref json-response 'expires_in 3600)))
          (begin
            (printf "Token exchange failed: ~a~n" json-response)
            #f)))))

(define (refresh-access-token refresh-token client-id client-secret)
  (printf "Refreshing access token...~n")
  (let* ([post-data
          (alist->form-urlencoded
           (list (cons 'client_id client-id)
                 (cons 'client_secret client-secret)
                 (cons 'refresh_token refresh-token)
                 (cons 'grant_type "refresh_token")))]
         [response
          (post-pure-port
           (string->url GOOGLE-TOKEN-URL)
           (string->bytes/utf-8 post-data)
           (list "Content-Type: application/x-www-form-urlencoded"))])
    (let ([json-response (read-json response)])
      (if (hash-has-key? json-response 'access_token)
          (hash 'access_token (hash-ref json-response 'access_token)
                'refresh_token refresh-token  ; Google doesn't always return a new one
                'expires_at (+ (current-seconds)
                               (hash-ref json-response 'expires_in 3600)))
          (begin
            (printf "Token refresh failed: ~a~n" json-response)
            #f)))))

;; ---- main entry point ----

;; Get a valid access token for the given email, handling the full
;; flow: check saved tokens → refresh → re-authorize as needed.
(define (get-valid-access-token email-address oauth2-creds)
  (let ([saved (load-tokens email-address)]
        [client-id (oauth2-details-client-id oauth2-creds)]
        [client-secret (oauth2-details-client-secret oauth2-creds)]
        [redirect-uri (oauth2-details-redirect-uri oauth2-creds)])

    (define (do-fresh-auth)
      (let ([auth-code (get-authorization-code client-id redirect-uri)])
        (let ([tokens (exchange-code-for-tokens auth-code client-id client-secret redirect-uri)])
          (when tokens
            (save-tokens! email-address tokens))
          tokens)))

    (cond
      ;; No saved tokens — need fresh authorization
      [(not saved)
       (printf "No saved tokens for ~a. Starting authorization flow...~n" email-address)
       (do-fresh-auth)]

      ;; Have tokens and access token is still valid (with 60s buffer)
      [(and (hash? saved)
            (> (hash-ref saved 'expires_at 0) (+ (current-seconds) 60)))
       (printf "Using saved access token for ~a~n" email-address)
       saved]

      ;; Have refresh token — try to refresh
      [(and (hash? saved) (hash-ref saved 'refresh_token #f))
       (let ([refreshed (refresh-access-token
                         (hash-ref saved 'refresh_token)
                         client-id client-secret)])
         (if refreshed
             (begin (save-tokens! email-address refreshed) refreshed)
             ;; Refresh failed — need fresh auth
             (do-fresh-auth)))]

      ;; Fallback — need fresh auth
      [else (do-fresh-auth)])))

;; Connect to Gmail IMAP using OAuth2. Returns an imap-connection.
(define (oauth2-connect-to-imap email-address oauth2-creds folder-name)
  (let ([tokens (get-valid-access-token email-address oauth2-creds)])
    (unless tokens
      (error 'oauth2-connect-to-imap "Could not obtain access token for ~a" email-address))

    (let ([access-token (hash-ref tokens 'access_token)]
          [hostname "imap.gmail.com"]
          [port-no 993])
      (printf "Connecting to Gmail IMAP for ~a...~n" email-address)
      (let-values
          ([(imap-connection messages# nu)
            (let-values ([(in out) (ssl-connect hostname port-no)])
              (imap-connect*
               in out
               email-address
               access-token
               folder-name
               #:try-tls? #t
               #:xoauth2? #t))])
        (printf "Connected! ~a messages, ~a new~n" messages# nu)
        imap-connection))))
