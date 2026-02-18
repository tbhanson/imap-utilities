#lang racket

;; Fetch mail headers from all accounts in the credentials file.
;;
;; Usage:
;;   racket fetch-all.rkt                    ; full fetch of INBOX for all accounts
;;   racket fetch-all.rkt --update           ; incremental fetch of INBOX for all accounts
;;   racket fetch-all.rkt "Sent" --update    ; specify folder
;;   racket fetch-all.rkt --all-digested     ; incremental update of every account+folder
;;                                           ;   that already has a saved digest
;;
;; Accounts/folders that fail (e.g. expired tokens) are skipped with a warning,
;; so one bad entry doesn't stop the rest.

(require
  "src/imap-email-account-credentials.rkt"
  "src/mailbox-digest.rkt")

(define (ensure-digest-dir-exists!)
  (let ([dir (default-digest-dir)])
    (unless (directory-exists? dir)
      (make-directory* dir)
      (printf "Created digest directory: ~a~n" dir))
    dir))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)])
    (let ([update? (if (member "--update" arg-list) #t #f)]
          [all-digested? (if (member "--all-digested" arg-list) #t #f)]
          [positional (filter (lambda (a) (not (string-prefix? a "--"))) arg-list)])
      (values (if (null? positional) "INBOX" (first positional))
              update?
              all-digested?))))

;; ---- fetching ----

(define (fetch-one-account credential folder-name update? output-dir)
  (let ([account-name (imap-email-account-credentials-accountname credential)]
        [email (imap-email-account-credentials-mailaddress credential)])
    (printf "~n========================================~n")
    (printf "  ~a (~a) / ~a~n" account-name email folder-name)
    (printf "========================================~n")
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (printf "ERROR fetching ~a / ~a: ~a~n" account-name folder-name (exn-message e))
            (printf "Skipping.~n"))])
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

;; ---- --all-digested: scan saved digests for (email, folder) pairs ----

;; Returns a list of (email . folder-name) pairs from all saved digests,
;; deduplicated, keeping only the latest digest per pair.
(define (all-digested-pairs)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (let ([seen (make-hash)])
          (for ([f (directory-list dir #:build? #t)]
                #:when (regexp-match? #rx"\\.ser$" (path->string f)))
            (with-handlers ([exn:fail?
                             (lambda (e)
                               (printf "Warning: could not read ~a: ~a~n"
                                       (file-name-from-path f) (exn-message e)))])
              (let* ([mbd (load-mailbox-digest-from-file f)]
                     [key (cons (mailbox-digest-mail-address mbd)
                                (mailbox-digest-folder-name mbd))])
                (hash-set! seen key #t))))
          (hash-keys seen))
        '())))

(define (do-all-digested-update creds output-dir)
  (let ([pairs (all-digested-pairs)])
    (printf "Found ~a previously digested account+folder combinations.~n" (length pairs))
    (when (null? pairs)
      (printf "No saved digests found. Fetch some first with:~n")
      (printf "  racket fetch.rkt <account-name>~n")
      (exit 0))

    ;; Build a reverse lookup: email -> credential
    (let ([email->credential (make-hash)])
      (for ([name (hash-keys creds)])
        (let ([cred (hash-ref creds name)])
          (hash-set! email->credential
                     (imap-email-account-credentials-mailaddress cred)
                     cred)))

      ;; Fetch each pair
      (for ([pair (sort pairs string<? #:key (lambda (p) (format "~a/~a" (car p) (cdr p))))])
        (let ([email (car pair)]
              [folder (cdr pair)])
          (let ([credential (hash-ref email->credential email #f)])
            (if credential
                (fetch-one-account credential folder #t output-dir)
                (printf "~nWARNING: no credential found for ~a, skipping.~n" email))))))))

;; ---- main ----

(define (main)
  (let-values ([(folder-name update? all-digested?) (parse-args (current-command-line-arguments))])
    (printf "Loading credentials...~n")
    (let ([creds (read-email-account-credentials-hash-from-file-named
                  (default-credentials-filepath))]
          [output-dir (ensure-digest-dir-exists!)])

      (if all-digested?
          ;; Update every previously digested account+folder
          (do-all-digested-update creds output-dir)

          ;; Normal mode: fetch one folder for each account
          (let ([account-names (sort (hash-keys creds) string<?)])
            (printf "Found ~a accounts. Fetching ~a for each~a.~n"
                    (length account-names)
                    folder-name
                    (if update? " (incremental)" ""))
            (for ([name account-names])
              (fetch-one-account (hash-ref creds name) folder-name update? output-dir))))

      (printf "~n========================================~n")
      (printf "  Done!~n")
      (printf "========================================~n"))))

(main)
