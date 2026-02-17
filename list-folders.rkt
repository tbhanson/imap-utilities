#lang racket

;; List all IMAP folders (mailboxes) for an account.
;;
;; Usage:
;;   racket list-folders.rkt <account-name>
;;   racket list-folders.rkt             ; lists all accounts

(require
  "src/imap-email-account-credentials.rkt"
  "src/connect-to-imap-account.rkt"
  "src/gmail-oauth2.rkt"
  "src/oauth2-details.rkt"
  net/imap
  openssl)

(define (connect-for-listing credential)
  (if (imap-email-account-credentials-xoauth2? credential)
      (let ([oauth2-creds (load-google-oauth2-details)]
            [email (imap-email-account-credentials-mailaddress credential)])
        (oauth2-connect-to-imap email oauth2-creds "INBOX"))
      (securely-connect-to-imap-account credential "INBOX")))

(define (entry->name entry)
  (let ([name (second entry)])
    (cond [(string? name) name]
          [(bytes? name) (bytes->string/utf-8 name)]
          [else (format "~a" name)])))

(define (has-children? entry)
  (member '|\HasChildren| (first entry)))

(define (list-folders-for credential)
  (let* ([account-name (imap-email-account-credentials-accountname credential)]
         [email (imap-email-account-credentials-mailaddress credential)]
         [imap-conn (connect-for-listing credential)]
         [top-level (imap-list-child-mailboxes imap-conn #f)])
    (printf "~n~a (~a):~n" account-name email)
    ;; Collect all folder names, recursing into containers like [Gmail]
    (let ([all-folders
           (for/fold ([result '()])
                     ([entry top-level])
             (let ([name (entry->name entry)])
               (if (has-children? entry)
                   ;; List children and include both parent and children
                   (let ([children (imap-list-child-mailboxes imap-conn name)])
                     (values (append result
                                     (list (format "~a  (container)" name))
                                     (map entry->name children))))
                   (values (append result (list name))))))])
      (for ([name (sort all-folders string<?)])
        (printf "  ~a~n" name)))
    (imap-disconnect imap-conn)))

(define (main)
  (let ([args (current-command-line-arguments)]
        [creds (read-email-account-credentials-hash-from-file-named
                (default-credentials-filepath))])
    (if (>= (vector-length args) 1)
        (let ([account-name (vector-ref args 0)])
          (unless (hash-has-key? creds account-name)
            (printf "No account named ~s. Available:~n" account-name)
            (for ([name (sort (hash-keys creds) string<?)])
              (printf "  ~a~n" name))
            (exit 1))
          (list-folders-for (hash-ref creds account-name)))
        (for ([name (sort (hash-keys creds) string<?)])
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (printf "~nERROR listing ~a: ~a~n" name (exn-message e)))])
            (list-folders-for (hash-ref creds name)))))))

(main)
