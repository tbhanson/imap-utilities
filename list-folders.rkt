#lang racket

;; List all IMAP folders (mailboxes) for an account.
;;
;; Usage:
;;   racket list-folders.rkt <account-name>
;;   racket list-folders.rkt <account-name> --counts
;;   racket list-folders.rkt                          ; all accounts
;;   racket list-folders.rkt --counts                 ; all accounts with counts
;;
;; --counts opens each folder to get the message count (slower).

(require
  "src/imap-email-account-credentials.rkt"
  "src/connect-to-imap-account.rkt"
  "src/gmail-oauth2.rkt"
  "src/oauth2-details.rkt"
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

(define (list-folders-for credential show-counts?)
  (let* ([account-name (imap-email-account-credentials-accountname credential)]
         [email (imap-email-account-credentials-mailaddress credential)]
         [imap-conn (connect-to credential "INBOX")]
         [folders (collect-folders imap-conn)])
    (imap-disconnect imap-conn)
    (printf "~n~a (~a):~n" account-name email)
    (let ([sorted (sort folders string<? #:key car)])
      (for ([f sorted])
        (let ([name (car f)]
              [selectable? (cdr f)])
          (cond
            [(not selectable?)
             (printf "  ~a  (container)~n" name)]
            [show-counts?
             (printf "  ~a" name)
             (flush-output)
             (let ([count (folder-message-count credential name)])
               (if count
                   (printf " (~a)~n" count)
                   (printf " (?)~n")))]
            [else
             (printf "  ~a~n" name)]))))))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([account-name #f]
        [counts? #f])
    (for ([arg (vector->list args)])
      (cond
        [(string=? arg "--counts") (set! counts? #t)]
        [else (set! account-name arg)]))
    (values account-name counts?)))

;; ---- main ----

(define (main)
  (let-values ([(account-name show-counts?) (parse-args (current-command-line-arguments))])
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
            (list-folders-for (hash-ref creds account-name) show-counts?))
          ;; All accounts
          (for ([name (sort (hash-keys creds) string<?)])
            (with-handlers ([exn:fail?
                             (lambda (e)
                               (printf "~nERROR listing ~a: ~a~n" name (exn-message e)))])
              (list-folders-for (hash-ref creds name) show-counts?)))))))

(main)
