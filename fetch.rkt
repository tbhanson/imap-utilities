#lang racket

;; Fetch mail headers from an IMAP account and save as a local digest.
;;
;; Usage:
;;   racket fetch.rkt <account-name> [<folder>] [<max-messages>]
;;   racket fetch.rkt <account-name> [<folder>] --update
;;
;; Examples:
;;   racket fetch.rkt "my-gmail"                     ; fetch all of INBOX
;;   racket fetch.rkt "my-gmail" "INBOX" 500         ; fetch first 500
;;   racket fetch.rkt "my-gmail" "INBOX" --update    ; only new since last fetch
;;   racket fetch.rkt "my-gmail" --update            ; same, INBOX is default
;;
;; With --update, the tool:
;;   1. Finds the most recent saved digest for this account+folder
;;   2. Fetches only messages newer than what's already saved
;;   3. Merges old + new into a combined digest and saves it
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

;; Parse args, separating flags (--update) from positional args.
(define (parse-args args)
  (let ([positional '()]
        [update? #f])
    (for ([arg (vector->list args)])
      (cond
        [(string=? arg "--update") (set! update? #t)]
        [else (set! positional (append positional (list arg)))]))
    (values positional update?)))

(define (main)
  (let ([args (current-command-line-arguments)])
    (when (< (vector-length args) 1)
      (printf "Usage: racket fetch.rkt <account-name> [<folder>] [<max-messages>]~n")
      (printf "       racket fetch.rkt <account-name> [<folder>] --update~n")
      (printf "~nAccount names in your credentials file:~n")
      (let ([creds (read-email-account-credentials-hash-from-file-named
                    (default-credentials-filepath))])
        (for ([name (sort (hash-keys creds) string<?)])
          (printf "  ~a~n" name)))
      (exit 1))

    (let-values ([(positional update?) (parse-args args)])
      (let ([account-name (first positional)]
            [folder-name (if (>= (length positional) 2)
                             (second positional)
                             "INBOX")]
            [max-messages (if (and (not update?) (>= (length positional) 3))
                              (string->number (third positional))
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
                [email (imap-email-account-credentials-mailaddress
                        (hash-ref creds account-name))])

            (if update?
                ;; ---- incremental fetch ----
                (do-incremental-fetch credential email folder-name)
                ;; ---- full fetch ----
                (do-full-fetch credential folder-name max-messages))))))))

(define (do-full-fetch credential folder-name max-messages)
  (let ([index-range (cons 1 (if (infinite? max-messages) 999999 max-messages))])
    (printf "Fetching headers from ~a / ~a ...~n"
            (imap-email-account-credentials-accountname credential)
            folder-name)
    (let ([digest (time (get-mailbox-digest credential folder-name index-range))])
      (printf "Fetched ~a message headers.~n" (mailbox-digest-count digest))
      (let* ([output-dir (ensure-digest-dir-exists!)]
             [saved-path (save-mailbox-digest digest output-dir)])
        (printf "Saved digest to:~n  ~a~n" saved-path)))))

(define (do-incremental-fetch credential email folder-name)
  (let ([previous-path (find-latest-digest-for email folder-name)])
    (if (not previous-path)
        ;; No previous digest — do a full fetch
        (begin
          (printf "No previous digest found for ~a / ~a. Doing full fetch.~n"
                  email folder-name)
          (do-full-fetch credential folder-name +inf.0))

        ;; Have a previous digest — fetch only new messages
        (begin
          (printf "Found previous digest: ~a~n" (file-name-from-path previous-path))
          (let* ([old-digest (time (load-mailbox-digest-from-file previous-path))]
                 [old-count (mailbox-digest-count old-digest)]
                 [old-uid-validity (mailbox-digest-uid-validity old-digest)]
                 [old-max-index (cdr (mailbox-digest-index-range old-digest))])
            (printf "Previous digest: ~a messages, index range 1-~a~n"
                    old-count old-max-index)

            ;; Fetch from one past our previous high index
            (let* ([start-index (+ old-max-index 1)]
                   [index-range (cons start-index 999999)])
              (printf "Fetching new messages starting at index ~a ...~n" start-index)
              (let ([new-digest (time (get-mailbox-digest credential folder-name index-range))])
                (let ([new-count (mailbox-digest-count new-digest)])
                  (cond
                    [(= new-count 0)
                     (printf "No new messages since last fetch.~n")]
                    [else
                     (printf "Fetched ~a new message headers.~n" new-count)

                     ;; Check uid-validity hasn't changed
                     (when (not (= old-uid-validity (mailbox-digest-uid-validity new-digest)))
                       (printf "WARNING: UID validity changed! Old digest may be stale.~n")
                       (printf "  Consider doing a full fetch instead.~n"))

                     ;; Merge and save
                     (let* ([merged (merge-mailbox-digests old-digest new-digest)]
                            [output-dir (ensure-digest-dir-exists!)]
                            [saved-path (save-mailbox-digest merged output-dir)])
                       (printf "Combined digest: ~a messages~n"
                               (mailbox-digest-count merged))
                       (printf "Saved to:~n  ~a~n" saved-path))])))))))))

(main)
