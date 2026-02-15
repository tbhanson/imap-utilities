#lang racket

;; Fetch mail headers from an IMAP account and save as a local digest.
;;
;; Usage:
;;   racket fetch.rkt <account-name> [<folder>] [<max-messages>]
;;
;; Examples:
;;   racket fetch.rkt "my-gmail"                    ; fetch all of INBOX
;;   racket fetch.rkt "my-gmail" "INBOX" 500        ; fetch first 500
;;   racket fetch.rkt "my-gmail" "[Gmail]/All Mail"  ; different folder
;;
;; The account-name must match an entry in ~/.imap_secrets/credentials.
;; The digest is saved to ~/.imap_secrets/digests/.

(require
  "src/imap-email-account-credentials.rkt"
  "src/mailbox-digest.rkt")

(define (ensure-digest-dir-exists!)
  (let ([dir (default-digest-dir)])
    (unless (directory-exists? dir)
      (make-directory* dir)
      (printf "Created digest directory: ~a~n" dir))
    dir))

(define (main)
  (let ([args (current-command-line-arguments)])
    (when (< (vector-length args) 1)
      (printf "Usage: racket fetch.rkt <account-name> [<folder>] [<max-messages>]~n")
      (printf "~nAccount names in your credentials file:~n")
      (let ([creds (read-email-account-credentials-hash-from-file-named
                    (default-credentials-filepath))])
        (for ([name (sort (hash-keys creds) string<?)])
          (printf "  ~a~n" name)))
      (exit 1))

    (let ([account-name (vector-ref args 0)]
          [folder-name (if (>= (vector-length args) 2)
                           (vector-ref args 1)
                           "INBOX")]
          [max-messages (if (>= (vector-length args) 3)
                            (string->number (vector-ref args 2))
                            +inf.0)])

      (printf "Loading credentials...~n")
      (let ([creds (read-email-account-credentials-hash-from-file-named
                    (default-credentials-filepath))])
        (unless (hash-has-key? creds account-name)
          (printf "Error: no account named ~s in credentials file.~n" account-name)
          (printf "Available accounts: ~a~n"
                  (string-join (sort (hash-keys creds) string<?) ", "))
          (exit 1))

        (let ([credential (hash-ref creds account-name)]
              [index-range (cons 1 (if (infinite? max-messages)
                                       999999
                                       max-messages))])
          (printf "Fetching headers from ~a / ~a ...~n" account-name folder-name)
          (let ([digest (time (get-mailbox-digest credential folder-name index-range))])
            (printf "Fetched ~a message headers.~n" (mailbox-digest-count digest))

            (let* ([output-dir (ensure-digest-dir-exists!)]
                   [saved-path (save-mailbox-digest digest output-dir)])
              (printf "Saved digest to:~n  ~a~n" saved-path))))))))

(main)
