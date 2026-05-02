#lang racket

;; Diagnostic: check whether messages from a sender exist in
;; INBOX vs [Google Mail]/Alle Nachrichten (All Mail) vs Trash.
;;
;; Useful for verifying that purges actually deleted messages, and
;; for understanding the gap between INBOX and All Mail counts.
;;
;; Usage:
;;   racket check-allmail.rkt <from-address>
;;   racket check-allmail.rkt <from-address> --account <substr>
;;
;; By default checks all Gmail (xoauth2) accounts. Pass --account
;; with a substring of the account name or email to limit the check.

(require
  "src/imap-email-account-credentials.rkt"
  "src/connect-to-imap-account.rkt"
  "src/gmail-oauth2.rkt"
  "src/oauth2-details.rkt"
  net/imap
  net/head
  openssl)

(define (default-credentials-filepath)
  (build-path (find-system-path 'home-dir) ".imap_secrets" "credentials"))

(define (load-credentials)
  (read-email-account-credentials-hash-from-file-named
   (default-credentials-filepath)))

(define (connect-to credential folder-name)
  (if (imap-email-account-credentials-xoauth2? credential)
      (let ([oauth2-creds (load-google-oauth2-details)]
            [email (imap-email-account-credentials-mailaddress credential)])
        (oauth2-connect-to-imap email oauth2-creds folder-name))
      (securely-connect-to-imap-account credential folder-name)))

;; Return list of (name . credential) pairs, optionally filtered by substring
;; matching either the account name or the email address.
(define (matching-credentials creds substr)
  (let ([sub (and substr (string-downcase substr))])
    (for/list ([name (sort (hash-keys creds) string<?)]
               #:when
               (let* ([credential (hash-ref creds name)]
                      [email (imap-email-account-credentials-mailaddress credential)])
                 (or (not sub)
                     (string-contains? (string-downcase name) sub)
                     (string-contains? (string-downcase email) sub))))
      (cons name (hash-ref creds name)))))

(define (count-matches imap-conn target-from sample-size)
  (let ([msg-count (imap-messages imap-conn)])
    (let* ([start (max 1 (- msg-count (- sample-size 1)))]
           [indices (for/list ([i (in-range start (+ msg-count 1))]) i)])
      (values msg-count
              (length indices)
              (for/sum ([result (imap-get-messages imap-conn indices '(uid header))])
                (with-handlers ([exn:fail? (lambda (e) 0)])
                  (let* ([header (second result)]
                         [from-bytes (extract-field #"from" header)]
                         [from-str (if from-bytes
                                       (string-downcase (bytes->string/utf-8 from-bytes))
                                       "")])
                    (if (string-contains? from-str target-from) 1 0))))))))

;; Gmail localizes its special folder names. We try the common
;; variants and use whichever the server accepts.
(define ALL-MAIL-FOLDERS
  '("[Gmail]/All Mail"
    "[Google Mail]/Alle Nachrichten"
    "[Gmail]/Alle Nachrichten"
    "[Google Mail]/All Mail"))

(define TRASH-FOLDERS
  '("[Gmail]/Trash"
    "[Google Mail]/Papierkorb"
    "[Gmail]/Papierkorb"
    "[Google Mail]/Trash"))

;; Try each folder name in order; check the first that works.
;; Silently skips on NONEXISTENT errors.
(define (check-folder-variants credential folder-names target-from sample-size label)
  (let loop ([remaining folder-names])
    (cond
      [(null? remaining)
       (printf "    ~a: not found in any expected location~n" label)]
      [else
       (let ([result
              (with-handlers
                  ([exn:fail?
                    (lambda (e)
                      (let ([msg (exn-message e)])
                        (if (regexp-match? #rx"NONEXISTENT|Unknown Mailbox" msg)
                            'try-next
                            (begin (printf "    ~a: ERROR: ~a~n" label msg)
                                   'failed))))])
                (let ([imap-conn (connect-to credential (car remaining))])
                  (let-values ([(total sampled hits)
                                (count-matches imap-conn target-from sample-size)])
                    (printf "    ~a (~a): ~a messages total; ~a/~a sampled match ~a~n"
                            label (car remaining) total hits sampled target-from))
                  (imap-disconnect imap-conn)
                  'ok))])
         (when (eq? result 'try-next)
           (loop (cdr remaining))))])))

(define (check-folder credential folder-name target-from sample-size)
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (printf "    ~a: ERROR: ~a~n" folder-name (exn-message e)))])
    (let ([imap-conn (connect-to credential folder-name)])
      (let-values ([(total sampled hits) (count-matches imap-conn target-from sample-size)])
        (printf "    ~a: ~a messages total; ~a/~a sampled match ~a~n"
                folder-name total hits sampled target-from))
      (imap-disconnect imap-conn))))

(define (parse-args args)
  (let ([arg-list (vector->list args)]
        [target #f]
        [account-filter #f])
    (let loop ([remaining arg-list] [positional '()])
      (cond
        [(null? remaining)
         (when (not (null? positional))
           (set! target (car (reverse positional))))]
        [(and (string=? (car remaining) "--account")
              (not (null? (cdr remaining))))
         (set! account-filter (cadr remaining))
         (loop (cddr remaining) positional)]
        [else (loop (cdr remaining) (cons (car remaining) positional))]))
    (values target account-filter)))

(define (main)
  (let-values ([(target-from account-filter)
                (parse-args (current-command-line-arguments))])

    (unless target-from
      (printf "Usage: racket check-allmail.rkt <from-address> [--account <substr>]~n")
      (exit 1))

    (let* ([target (string-downcase target-from)]
           [creds (load-credentials)]
           [matches (matching-credentials creds account-filter)]
           ;; Filter to OAuth2 accounts only — non-Gmail accounts don't have
           ;; the [Google Mail]/* folders this tool inspects
           [gmail-matches (filter (lambda (p)
                                    (imap-email-account-credentials-xoauth2?
                                     (cdr p)))
                                  matches)])

      (when (null? gmail-matches)
        (printf "No Gmail accounts ~a.~n"
                (if account-filter
                    (format "match '~a'" account-filter)
                    "configured"))
        (exit 0))

      (printf "~nChecking ~a Gmail account(s) for messages from ~a:~n"
              (length gmail-matches) target)

      (for ([pair gmail-matches])
        (let* ([name (car pair)]
               [credential (cdr pair)]
               [email (imap-email-account-credentials-mailaddress credential)])
          (printf "~n  ~a (~a)~n" name email)
          (check-folder credential "INBOX" target 500)
          (check-folder-variants credential ALL-MAIL-FOLDERS target 2000 "All Mail")
          (check-folder-variants credential TRASH-FOLDERS target 100 "Trash"))))))

(main)
