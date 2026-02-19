#lang racket

;; Find unread messages from known contacts across all saved digests.
;;
;; Usage:
;;   racket find-unread.rkt                              ; unread from known contacts
;;   racket find-unread.rkt --all                        ; unread from anyone
;;   racket find-unread.rkt --from someone@example.com   ; unread from a specific sender
;;   racket find-unread.rkt --category family            ; unread from a contact category
;;   racket find-unread.rkt --account "my-gmail"         ; only search one account
;;   racket find-unread.rkt --category friends --account "my-gmail"  ; combine filters
;;   racket find-unread.rkt --categories                 ; list available categories
;;
;; Scans all saved digests (excluding sent-mail folders) and shows
;; messages that don't have the \Seen flag.

(require
  "src/imap-email-account-credentials.rkt"
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  "src/mail-digest.rkt"
  "src/known-contacts.rkt"
  gregor)

;; ---- helpers ----

(define (message-seen? hdr)
  (member '|\\Seen| (main-mail-header-parts-flags hdr)))

(define (message-from-addr hdr)
  (with-handlers ([exn:fail? (lambda (e) "")])
    ((mail-digest-from-header-parts hdr) 'from-addr)))

;; ---- loading ----

;; Load all non-sent digests (latest per account+folder).
;; If account-email is given, only load digests for that account.
(define (load-inbox-digests account-email)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (let ([all-files
               (for/list ([f (directory-list dir #:build? #t)]
                          #:when (regexp-match? #rx"\\.ser$" (path->string f)))
                 f)])
          (let ([by-key (make-hash)])
            (for ([f all-files])
              (with-handlers ([exn:fail?
                               (lambda (e)
                                 (printf "Warning: could not read ~a~n"
                                         (file-name-from-path f)))])
                (let ([mbd (load-mailbox-digest-from-file f)])
                  ;; Skip sent-mail digests
                  (unless (regexp-match? #rx"(?i:sent|gesendet|envoy|inviati|enviados|verzonden)"
                                         (mailbox-digest-folder-name mbd))
                    ;; Filter by account if specified
                    (when (or (not account-email)
                              (string=? (mailbox-digest-mail-address mbd) account-email))
                      (let ([key (cons (mailbox-digest-mail-address mbd)
                                       (mailbox-digest-folder-name mbd))])
                        (let ([existing (hash-ref by-key key #f)])
                          (when (or (not existing)
                                    (datetime>? (mailbox-digest-timestamp mbd)
                                                (mailbox-digest-timestamp existing)))
                            (hash-set! by-key key mbd)))))))))
            (hash-values by-key)))
        '())))

;; Resolve an account name to an email address
(define (account-name->email account-name)
  (let ([creds (read-email-account-credentials-hash-from-file-named
                (default-credentials-filepath))])
    (if (hash-has-key? creds account-name)
        (imap-email-account-credentials-mailaddress (hash-ref creds account-name))
        (begin
          (printf "No account named ~s. Available:~n" account-name)
          (for ([name (sort (hash-keys creds) string<?)])
            (printf "  ~a~n" name))
          (exit 1)))))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)]
        [show-all? #f]
        [from-filter #f]
        [category-filter #f]
        [account-filter #f]
        [list-categories? #f])
    (let loop ([remaining arg-list])
      (cond
        [(null? remaining) (void)]
        [(string=? (car remaining) "--all")
         (set! show-all? #t)
         (loop (cdr remaining))]
        [(string=? (car remaining) "--categories")
         (set! list-categories? #t)
         (loop (cdr remaining))]
        [(and (string=? (car remaining) "--from")
              (not (null? (cdr remaining))))
         (set! from-filter (string-downcase (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--category")
              (not (null? (cdr remaining))))
         (set! category-filter (cadr remaining))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--account")
              (not (null? (cdr remaining))))
         (set! account-filter (cadr remaining))
         (loop (cddr remaining))]
        [else (loop (cdr remaining))]))
    (values show-all? from-filter category-filter account-filter list-categories?)))

;; ---- main ----

(define (main)
  (let-values ([(show-all? from-filter category-filter account-filter list-categories?)
                (parse-args (current-command-line-arguments))])

    (let ([categorized (load-known-contacts-categorized (default-known-contacts-filepath))]
          [known-set (load-known-contacts (default-known-contacts-filepath))])

      ;; --categories: just list categories and exit
      (when list-categories?
        (let ([cats (contact-categories categorized)])
          (if (null? cats)
              (printf "No categories found in known-contacts file.~n")
              (begin
                (printf "Categories in known-contacts file:~n")
                (for ([cat cats])
                  (printf "  ~a (~a contacts)~n"
                          cat
                          (set-count (contacts-in-category categorized cat))))))
        (exit 0)))

      ;; Build the filter set based on flags
      (let ([filter-set
             (cond
               [show-all? #f]  ; #f means match everyone
               [from-filter (set from-filter)]
               [category-filter
                (let ([cat-contacts (contacts-in-category categorized category-filter)])
                  (when (set-empty? cat-contacts)
                    (printf "No contacts found in category ~s.~n" category-filter)
                    (printf "Available categories: ~a~n"
                            (string-join (contact-categories categorized) ", "))
                    (exit 1))
                  cat-contacts)]
               [else known-set])]
            [filter-label
             (cond
               [show-all? "anyone"]
               [from-filter (format "~a" from-filter)]
               [category-filter (format "category: ~a" category-filter)]
               [else "known contacts"])])

        ;; Resolve account filter
        (let ([account-email
               (if account-filter
                   (account-name->email account-filter)
                   #f)])

          (when (and (not show-all?) (not from-filter) (not category-filter)
                     (set-empty? known-set))
            (printf "No known-contacts file found. Use --all to see all unread,~n")
            (printf "or create ~a first.~n" (default-known-contacts-filepath))
            (exit 0))

          (printf "Loading digests~a...~n"
                  (if account-email (format " for ~a" account-email) ""))
          (let ([digests (load-inbox-digests account-email)])

            (when (null? digests)
              (printf "No digests found.~n")
              (exit 0))

            (printf "Scanning ~a digest(s) for unread messages (~a)...~n~n"
                    (length digests) filter-label)

            (let ([total-unread 0]
                  [total-matching 0])

              (for ([mbd (sort digests string<?
                               #:key (lambda (d) (format "~a/~a"
                                                         (mailbox-digest-mail-address d)
                                                         (mailbox-digest-folder-name d))))])
                (let ([account (mailbox-digest-mail-address mbd)]
                      [folder (mailbox-digest-folder-name mbd)]
                      [unread-matches '()])

                  (for ([hdr (mailbox-digest-mail-headers mbd)])
                    (unless (message-seen? hdr)
                      (set! total-unread (add1 total-unread))
                      (let ([from (message-from-addr hdr)])
                        (when (or (not filter-set)  ; --all
                                  (set-member? filter-set (string-downcase from)))
                          (set! total-matching (add1 total-matching))
                          (set! unread-matches
                                (cons (list from
                                            (main-mail-header-parts-subj hdr)
                                            (main-mail-header-parts-date-string hdr)
                                            (known-contact-category categorized from))
                                      unread-matches))))))

                  (when (not (null? unread-matches))
                    (printf "~a / ~a (~a unread matching ~a):~n"
                            account folder
                            (length unread-matches)
                            filter-label)
                    (for ([match (reverse unread-matches)])
                      (let ([from (first match)]
                            [subj (second match)]
                            [date (third match)]
                            [cat (fourth match)])
                        (printf "  ~a  ~a~a~n    ~a~n"
                                date from
                                (if cat (format "  [~a]" cat) "")
                                (if (string=? subj "") "(no subject)" subj))))
                    (newline))))

              (printf "======================================================================~n")
              (printf "  Total unread across scanned digests: ~a~n" total-unread)
              (printf "  Matching unread (~a): ~a~n" filter-label total-matching)
              (printf "======================================================================~n"))))))))

(main)
