#lang racket

;; List all IMAP folders (mailboxes) for an account.
;;
;; Usage:
;;   racket list-folders.rkt <account-name>
;;   racket list-folders.rkt <account-name> --counts
;;   racket list-folders.rkt <account-name> --gaps
;;   racket list-folders.rkt <account-name> --fetch-gaps
;;   racket list-folders.rkt                             ; all accounts
;;   racket list-folders.rkt --counts                    ; all accounts with counts
;;   racket list-folders.rkt --gaps                      ; show unfetched non-empty folders
;;   racket list-folders.rkt --fetch-gaps                ; generate fetch commands for gaps (bash script we can edit to fetch folders we haven't digested yet)
;;
;; --counts      opens each folder to get the message count (slower).
;; --gaps        like --counts, but only shows non-empty folders that have
;;               no saved digest — i.e. folders you haven't fetched yet.
;; --fetch-gaps  like --gaps, but outputs ready-to-run fetch commands.
;;               Pipe to a file to create a script:
;;                 racket list-folders.rkt --fetch-gaps > fetch-gaps.sh

(require
  "src/imap-email-account-credentials.rkt"
  "src/connect-to-imap-account.rkt"
  "src/gmail-oauth2.rkt"
  "src/oauth2-details.rkt"
  "src/mailbox-digest.rkt"
  net/imap
  openssl)

;; ---- connection helpers ----

(define (connect-to credential folder-name)
  (if (imap-email-account-credentials-xoauth2? credential)
      (let ([oauth2-creds (load-google-oauth2-details)]
            [email (imap-email-account-credentials-mailaddress credential)])
        (oauth2-connect-to-imap email oauth2-creds folder-name))
      (securely-connect-to-imap-account credential folder-name)))

;; Get message count for a folder. Returns count or #f on error.
(define (folder-message-count credential folder-name)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (let ([imap-conn (connect-to credential folder-name)])
      (let ([count (imap-messages imap-conn)])
        (imap-disconnect imap-conn)
        count))))

;; ---- folder listing ----

(define (entry->name entry)
  (let ([name (second entry)])
    (cond [(string? name) name]
          [(bytes? name) (bytes->string/utf-8 name)]
          [else (format "~a" name)])))

(define (has-children? entry)
  (member '|\HasChildren| (first entry)))

(define (noselect? entry)
  (member '|\Noselect| (first entry)))

(define (collect-folders imap-conn)
  (let ([top-level (imap-list-child-mailboxes imap-conn #f)])
    (for/fold ([result '()])
              ([entry top-level])
      (let ([name (entry->name entry)]
            [selectable? (not (noselect? entry))])
        (if (has-children? entry)
            (let ([children (imap-list-child-mailboxes imap-conn name)])
              (values (append result
                              (list (cons name selectable?))
                              (map (lambda (c) (cons (entry->name c) (not (noselect? c))))
                                   children))))
            (values (append result (list (cons name selectable?)))))))))

;; Shell-escape a string for use in a command
(define (shell-escape s)
  (format "\"~a\"" (regexp-replace* #rx"\"" s "\\\\\"")))

(define (list-folders-for credential mode)
  ;; mode is one of: 'plain, 'counts, 'gaps, 'fetch-gaps
  (let* ([account-name (imap-email-account-credentials-accountname credential)]
         [email (imap-email-account-credentials-mailaddress credential)]
         [imap-conn (connect-to credential "INBOX")]
         [folders (collect-folders imap-conn)])
    (imap-disconnect imap-conn)

    (unless (eq? mode 'fetch-gaps)
      (printf "~n~a (~a):~n" account-name email))

    (let ([sorted (sort folders string<? #:key car)]
          [gap-count 0])
      (for ([f sorted])
        (let ([name (car f)]
              [selectable? (cdr f)])
          (cond
            [(not selectable?)
             (when (eq? mode 'plain)
               (printf "  ~a  (container)~n" name))]

            [(memq mode '(counts gaps fetch-gaps))
             (let ([count (folder-message-count credential name)])
               (case mode
                 [(counts)
                  (printf "  ~a" name)
                  (flush-output)
                  (if count
                      (printf " (~a)~n" count)
                      (printf " (?)~n"))]

                 [(gaps)
                  (when (and count (> count 0))
                    (let ([has-digest? (find-latest-digest-for email name)])
                      (unless has-digest?
                        (set! gap-count (add1 gap-count))
                        (printf "  ~a (~a messages) — NO DIGEST~n" name count))))]

                 [(fetch-gaps)
                  (when (and count (> count 0))
                    (let ([has-digest? (find-latest-digest-for email name)])
                      (unless has-digest?
                        (set! gap-count (add1 gap-count))
                        (printf "racket fetch.rkt ~a ~a  # ~a messages~n"
                                (shell-escape account-name)
                                (shell-escape name)
                                count))))]))]

            [else
             (printf "  ~a~n" name)])))

      (when (and (memq mode '(gaps fetch-gaps)) (= gap-count 0))
        (if (eq? mode 'fetch-gaps)
            (printf "# ~a (~a): all non-empty folders have local digests~n" account-name email)
            (printf "  (all non-empty folders have local digests)~n"))))))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)])
    (let ([positional (filter (lambda (a) (not (string-prefix? a "--"))) arg-list)])
      (values
       (if (null? positional) #f (first positional))
       (cond
         [(member "--fetch-gaps" arg-list) 'fetch-gaps]
         [(member "--gaps" arg-list)       'gaps]
         [(member "--counts" arg-list)     'counts]
         [else                             'plain])))))

;; ---- main ----

(define (main)
  (let-values ([(account-name mode) (parse-args (current-command-line-arguments))])

    (when (eq? mode 'fetch-gaps)
      (printf "#!/bin/bash~n")
      (printf "# Generated by: racket list-folders.rkt --fetch-gaps~n")
      (printf "# Fetch commands for non-empty folders with no local digest.~n")
      (printf "# Review and delete any lines you don't want, then run with:~n")
      (printf "#   bash fetch-gaps.sh~n~n"))

    (let ([creds (read-email-account-credentials-hash-from-file-named
                  (default-credentials-filepath))])
      (if account-name
          ;; One account
          (begin
            (unless (hash-has-key? creds account-name)
              (printf "No account named ~s. Available:~n" account-name)
              (for ([name (sort (hash-keys creds) string<?)])
                (printf "  ~a~n" name))
              (exit 1))
            (list-folders-for (hash-ref creds account-name) mode))
          ;; All accounts
          (for ([name (sort (hash-keys creds) string<?)])
            (with-handlers ([exn:fail?
                             (lambda (e)
                               (printf "~nERROR listing ~a: ~a~n" name (exn-message e)))])
              (list-folders-for (hash-ref creds name) mode)))))))

(main)
