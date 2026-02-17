#lang racket

;; Fetch mail headers from all accounts in the credentials file.
;;
;; Usage:
;;   racket fetch-all.rkt                  ; full fetch of INBOX for all accounts
;;   racket fetch-all.rkt --update         ; incremental fetch for all accounts
;;   racket fetch-all.rkt "INBOX" --update ; specify folder
;;
;; Accounts that fail (e.g. expired tokens) are skipped with a warning,
;; so one bad account doesn't stop the rest.

(require
  "src/imap-email-account-credentials.rkt"
  "src/mailbox-digest.rkt")

(define (ensure-digest-dir-exists!)
  (let ([dir (default-digest-dir)])
    (unless (directory-exists? dir)
      (make-directory* dir)
      (printf "Created digest directory: ~a~n" dir))
    dir))

(define (parse-args args)
  (let ([folder "INBOX"]
        [update? #f])
    (for ([arg (vector->list args)])
      (cond
        [(string=? arg "--update") (set! update? #t)]
        [else (set! folder arg)]))
    (values folder update?)))

(define (fetch-one-account credential folder-name update? output-dir)
  (let ([account-name (imap-email-account-credentials-accountname credential)]
        [email (imap-email-account-credentials-mailaddress credential)])
    (printf "~n========================================~n")
    (printf "  ~a (~a)~n" account-name email)
    (printf "========================================~n")
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (printf "ERROR fetching ~a: ~a~n" account-name (exn-message e))
            (printf "Skipping this account.~n"))])
      (if (and update? (find-latest-digest-for email folder-name))
          ;; Incremental
          (let* ([previous-path (find-latest-digest-for email folder-name)]
                 [old-digest (load-mailbox-digest-from-file previous-path)]
                 [old-max-index (cdr (mailbox-digest-index-range old-digest))]
                 [index-range (cons (+ old-max-index 1) 999999)])
            (printf "Previous digest: ~a messages. Fetching new...~n"
                    (mailbox-digest-count old-digest))
            (let ([new-digest (get-mailbox-digest credential folder-name index-range)])
              (if (= (mailbox-digest-count new-digest) 0)
                  (printf "No new messages.~n")
                  (let* ([merged (merge-mailbox-digests old-digest new-digest)]
                         [saved-path (save-mailbox-digest merged output-dir)])
                    (printf "~a new. Combined: ~a messages.~n"
                            (mailbox-digest-count new-digest)
                            (mailbox-digest-count merged))
                    (printf "Saved to: ~a~n" (file-name-from-path saved-path))))))
          ;; Full fetch
          (let* ([index-range (cons 1 999999)]
                 [digest (get-mailbox-digest credential folder-name index-range)]
                 [saved-path (save-mailbox-digest digest output-dir)])
            (printf "Fetched ~a messages.~n" (mailbox-digest-count digest))
            (printf "Saved to: ~a~n" (file-name-from-path saved-path)))))))

(define (main)
  (let-values ([(folder-name update?) (parse-args (current-command-line-arguments))])
    (printf "Loading credentials...~n")
    (let ([creds (read-email-account-credentials-hash-from-file-named
                  (default-credentials-filepath))]
          [output-dir (ensure-digest-dir-exists!)])
      (let ([account-names (sort (hash-keys creds) string<?)])
        (printf "Found ~a accounts. Fetching ~a for each~a.~n"
                (length account-names)
                folder-name
                (if update? " (incremental)" ""))
        (for ([name account-names])
          (fetch-one-account (hash-ref creds name) folder-name update? output-dir))

        (printf "~n========================================~n")
        (printf "  Done! ~a accounts processed.~n" (length account-names))
        (printf "========================================~n")))))

(main)
