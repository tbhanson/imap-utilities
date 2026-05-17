#lang racket

;; Find unread messages from known contacts across all saved digests.
;;
;; Default behavior treats anyone in known-contacts OR in derived-contacts
;; (people you've ever sent mail to) as a "contact" — so unread mail from
;; people you've corresponded with is surfaced regardless of where they
;; first wrote to you.
;;
;; By default, mail from your own addresses (any address in credentials)
;; is excluded. Use --include-self to disable that.
;;
;; Usage:
;;   racket find-unread.rkt                              ; unread from contacts
;;   racket find-unread.rkt --all                        ; unread from anyone
;;   racket find-unread.rkt --from someone@example.com   ; unread from a specific sender
;;   racket find-unread.rkt --category family            ; unread from a contact category
;;   racket find-unread.rkt --account "my-gmail"         ; only search one account
;;   racket find-unread.rkt --year 2024                  ; only messages from 2024
;;   racket find-unread.rkt --since 2023-01-01           ; messages on or after date
;;   racket find-unread.rkt --before 2024-07-01          ; messages before date
;;   racket find-unread.rkt --category family --year 2025  ; combine filters
;;   racket find-unread.rkt --categories                 ; list available categories
;;   racket find-unread.rkt --include-self               ; show mail from my own addresses too
;;   racket find-unread.rkt --exclude-pattern '@lists?\\.' ; exclude addresses matching regex
;;   racket find-unread.rkt --exclude-pattern noreply --exclude-pattern listserv
;;   racket find-unread.rkt --exclude-category moi        ; exclude all addresses in a category
;;   racket find-unread.rkt --exclude-category moi --exclude-category lists
;;   racket find-unread.rkt --by-sender                   ; alphabetical, with counts and date ranges
;;   racket find-unread.rkt --newest-first                ; flat reverse-chronological across accounts
;;
;; --exclude-pattern is a Perl-style regex applied to the lowercased
;; sender address. Multiple --exclude-pattern flags are combined as OR.
;;
;; --exclude-category names a category from known-contacts.txt and
;; excludes all addresses tagged with that category. Repeatable.
;;
;; --category only matches the categorized portion of known-contacts.txt;
;; derived contacts have no categories and are therefore ignored when
;; --category is used.
;;
;; Date filters can be combined: --since 2024-01-01 --before 2024-07-01
;; gives the first half of 2024.
;;
;; Scans all saved digests (excluding sent-mail folders) and shows
;; messages that don't have the \Seen flag.

(require
  "src/imap-email-account-credentials.rkt"
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  "src/mail-digest.rkt"
  "src/known-contacts.rkt"
  "src/parse-mail-dates.rkt"
  "src/utils.rkt"
  gregor)

(handle-broken-pipe)

;; ---- helpers ----

(define (message-seen? hdr)
  (member '|\\Seen| (main-mail-header-parts-flags hdr)))

(define (message-from-addr hdr)
  (extract-from-addr (main-mail-header-parts-from hdr)))

(define (message-date hdr)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (possible-parse-date-time-string (main-mail-header-parts-date-string hdr))))

(define (message-year hdr)
  (or (main-mail-header-parts-parsed-year hdr)
      (let ([d (message-date hdr)])
        (and d (->year d)))))

;; Parse a date string like "2024-01-15" into a gregor date
(define (parse-date-arg s)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Could not parse date ~s. Use format YYYY-MM-DD.~n" s)
                     (exit 1))])
    (parse-date s "yyyy-MM-dd")))

;; Check whether a message date falls within the specified filters
(define (date-matches? hdr year-filter since-filter before-filter)
  (cond
    ;; No date filters — everything matches
    [(and (not year-filter) (not since-filter) (not before-filter)) #t]
    ;; Year filter: use pre-computed field
    [(and year-filter (not since-filter) (not before-filter))
     (let ([yr (message-year hdr)])
       (and yr (= yr year-filter)))]
    ;; Date range filters: use epoch if available
    [else
     (let ([epoch (main-mail-header-parts-parsed-epoch hdr)])
       (if epoch
           (and (or (not year-filter)
                    (let ([yr (main-mail-header-parts-parsed-year hdr)])
                      (and yr (= yr year-filter))))
                (or (not since-filter)
                    (>= epoch (->posix (datetime (->year since-filter) (->month since-filter) (->day since-filter)))))
                (or (not before-filter)
                    (< epoch (->posix (datetime (->year before-filter) (->month before-filter) (->day before-filter))))))
           ;; Fallback: full parsing
           (let ([d (message-date hdr)])
             (if (not d)
                 #f
                 (let ([msg-date (->date d)])
                   (and (or (not since-filter)
                            (date>=? msg-date since-filter))
                        (or (not before-filter)
                            (date<? msg-date before-filter))))))))]))

;; ---- loading ----

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
                  (unless (regexp-match? #rx"(?i:sent|gesendet|envoy|inviati|enviados|verzonden)"
                                         (mailbox-digest-folder-name mbd))
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
        [year-filter #f]
        [since-filter #f]
        [before-filter #f]
        [list-categories? #f]
        [exclude-patterns '()]
        [exclude-categories '()]
        [include-self? #f]
        [view-mode 'grouped])
    (let loop ([remaining arg-list])
      (cond
        [(null? remaining) (void)]
        [(string=? (car remaining) "--all")
         (set! show-all? #t)
         (loop (cdr remaining))]
        [(string=? (car remaining) "--categories")
         (set! list-categories? #t)
         (loop (cdr remaining))]
        [(string=? (car remaining) "--include-self")
         (set! include-self? #t)
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
        [(and (string=? (car remaining) "--year")
              (not (null? (cdr remaining))))
         (set! year-filter (string->number (cadr remaining)))
         (loop (cddr remaining))]
        [(and (or (string=? (car remaining) "--since")
                  (string=? (car remaining) "--after"))
              (not (null? (cdr remaining))))
         (set! since-filter (parse-date-arg (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--before")
              (not (null? (cdr remaining))))
         (set! before-filter (parse-date-arg (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--exclude-pattern")
              (not (null? (cdr remaining))))
         (set! exclude-patterns (cons (cadr remaining) exclude-patterns))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--exclude-category")
              (not (null? (cdr remaining))))
         (set! exclude-categories (cons (cadr remaining) exclude-categories))
         (loop (cddr remaining))]
        [(string=? (car remaining) "--by-sender")
         (set! view-mode 'by-sender)
         (loop (cdr remaining))]
        [(string=? (car remaining) "--newest-first")
         (set! view-mode 'newest-first)
         (loop (cdr remaining))]
        [else (loop (cdr remaining))]))
    (values show-all? from-filter category-filter account-filter
            year-filter since-filter before-filter list-categories?
            (reverse exclude-patterns) (reverse exclude-categories)
            include-self? view-mode)))

;; ---- rendering ----

(define (format-match-line m)
  (let ([from (hash-ref m 'from)]
        [subj (hash-ref m 'subj)]
        [date (hash-ref m 'date-string)]
        [cat (hash-ref m 'category)])
    (format "  ~a  ~a~a~n    ~a~n"
            date from
            (if cat (format "  [~a]" cat) "")
            (if (string=? subj "") "(no subject)" subj))))

;; Default view: grouped by account/folder (preserves the original
;; output style). Within each folder, messages are in the order they
;; were scanned (chronological — oldest first).
(define (render-grouped all-matches filter-label)
  (let ([by-folder (make-hash)])
    (for ([m all-matches])
      (let ([key (cons (hash-ref m 'account) (hash-ref m 'folder))])
        (hash-update! by-folder key (lambda (lst) (cons m lst)) '())))
    (for ([key (sort (hash-keys by-folder) string<?
                     #:key (lambda (p) (format "~a/~a" (car p) (cdr p))))])
      (let ([account (car key)]
            [folder (cdr key)]
            [matches (hash-ref by-folder key)])
        (printf "~a / ~a (~a unread matching ~a):~n"
                account folder (length matches) filter-label)
        (for ([m (reverse matches)])
          (display (format-match-line m)))
        (newline)))))

;; Newest-first view: a single flat list across all accounts, sorted
;; by epoch descending. Useful for "what's recent and unread that I
;; should pay attention to."
(define (render-newest-first all-matches filter-label)
  (let ([sorted (sort all-matches >
                      #:key (lambda (m) (or (hash-ref m 'epoch) 0)))])
    (printf "Newest-first across all accounts (~a unread matching ~a):~n"
            (length sorted) filter-label)
    (for ([m sorted])
      (let ([account (hash-ref m 'account)])
        (printf "  [~a]~n" account)
        (display (format-match-line m))))
    (newline)))

;; By-sender view: alphabetical by sender, with counts and date ranges.
;; Useful for "who's been writing to me — and how much?"
(define (render-by-sender all-matches filter-label)
  (let ([by-sender (make-hash)])
    (for ([m all-matches])
      (let ([from (hash-ref m 'from)])
        (hash-update! by-sender from (lambda (lst) (cons m lst)) '())))

    (printf "By sender (~a unique senders, ~a unread matching ~a):~n~n"
            (hash-count by-sender) (length all-matches) filter-label)

    (let ([senders (sort (hash-keys by-sender) string<?)])
      (printf "  ~a  ~a  ~a~n"
              (~a "Count" #:min-width 6 #:align 'right)
              (~a "Date range" #:min-width 25)
              "Sender")
      (printf "  ~a  ~a  ~a~n"
              (make-string 6 #\-)
              (make-string 25 #\-)
              (make-string 50 #\-))
      (for ([sender senders])
        (let* ([msgs (hash-ref by-sender sender)]
               [epochs (filter values (map (lambda (m) (hash-ref m 'epoch)) msgs))]
               [date-range
                (cond
                  [(null? epochs) "(no parseable dates)"]
                  [(= 1 (length epochs))
                   (let ([dt (posix->datetime (first epochs))])
                     (~t dt "yyyy-MM-dd"))]
                  [else
                   (let ([oldest (apply min epochs)]
                         [newest (apply max epochs)])
                     (format "~a → ~a"
                             (~t (posix->datetime oldest) "yyyy-MM-dd")
                             (~t (posix->datetime newest) "yyyy-MM-dd")))])]
               [cat (hash-ref (first msgs) 'category)])
          (printf "  ~a  ~a  ~a~a~n"
                  (~a (length msgs) #:min-width 6 #:align 'right)
                  (~a date-range #:min-width 25)
                  sender
                  (if cat (format "  [~a]" cat) "")))))
    (newline)))

;; ---- main ----

(define (main)
  (let-values ([(show-all? from-filter category-filter account-filter
                 year-filter since-filter before-filter list-categories?
                 exclude-patterns exclude-categories include-self? view-mode)
                (parse-args (current-command-line-arguments))])

    (let* ([categorized (load-known-contacts-categorized (default-known-contacts-filepath))]
           [known-set (load-all-known-contacts)]
           ;; All my own email addresses, from credentials. Used to
           ;; suppress mail from myself unless --include-self is given
           ;; or --from explicitly targets one of my addresses.
           [self-addresses
            (with-handlers ([exn:fail? (lambda (e) (set))])
              (let ([creds (read-email-account-credentials-hash-from-file-named
                            (default-credentials-filepath))])
                (for/set ([name (hash-keys creds)])
                  (string-downcase
                   (imap-email-account-credentials-mailaddress
                    (hash-ref creds name))))))]
           ;; Addresses to exclude based on --exclude-category. We collect
           ;; the union of all addresses in the named categories.
           [excluded-by-category
            (let ([result (mutable-set)])
              (for ([cat exclude-categories])
                (let ([cat-contacts (contacts-in-category categorized cat)])
                  (when (set-empty? cat-contacts)
                    (printf "Warning: --exclude-category ~s matched no contacts~n" cat))
                  (for ([a (in-set cat-contacts)])
                    (set-add! result (string-downcase a)))))
              result)]
           ;; Compile exclude patterns into regexes
           [exclude-regexes
            (for/list ([p exclude-patterns])
              (with-handlers ([exn:fail?
                               (lambda (e)
                                 (printf "Warning: bad exclude pattern ~s: ~a~n"
                                         p (exn-message e))
                                 #f)])
                (pregexp p)))])

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
               [show-all? #f]
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
             (string-join
              (filter values
                      (list
                       (cond
                         [show-all? "anyone"]
                         [from-filter (format "from ~a" from-filter)]
                         [category-filter (format "category: ~a" category-filter)]
                         [else "known contacts"])
                       (and year-filter (format "year ~a" year-filter))
                       (and since-filter (format "since ~a" (~t since-filter "yyyy-MM-dd")))
                       (and before-filter (format "before ~a" (~t before-filter "yyyy-MM-dd")))))
              ", ")])

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
                  [total-matching 0]
                  ;; All matches across all digests, accumulated for
                  ;; final rendering. Each record is a hash with keys:
                  ;;   account, folder, from, subj, date-string, epoch, category
                  [all-matches '()])

              (for ([mbd (sort digests string<?
                               #:key (lambda (d) (format "~a/~a"
                                                         (mailbox-digest-mail-address d)
                                                         (mailbox-digest-folder-name d))))])
                (let ([account (mailbox-digest-mail-address mbd)]
                      [folder (mailbox-digest-folder-name mbd)]
                      [msg-count (length (mailbox-digest-mail-headers mbd))]
                      [folder-matches 0])

                  (eprintf "  scanning ~a / ~a (~a messages)... " account folder msg-count)
                  (flush-output (current-error-port))

                  (for ([hdr (mailbox-digest-mail-headers mbd)])
                    (unless (message-seen? hdr)
                      (set! total-unread (add1 total-unread))
                      (let ([from (message-from-addr hdr)])
                        (when (and
                               ;; Sender filter
                               (or (not filter-set)
                                   (set-member? filter-set (string-downcase from)))
                               ;; Date filter
                               (date-matches? hdr year-filter since-filter before-filter)
                               ;; Self-exclusion (skip if from one of my own
                               ;; addresses, unless --include-self or --from
                               ;; explicitly targets a self address).
                               (or include-self?
                                   from-filter
                                   (not (set-member? self-addresses
                                                     (string-downcase from))))
                               ;; Category exclusion (--exclude-category)
                               (or from-filter
                                   (not (set-member? excluded-by-category
                                                     (string-downcase from))))
                               ;; Exclude-pattern filter
                               (not (for/or ([rx exclude-regexes])
                                      (and rx (regexp-match? rx from)))))
                          (set! total-matching (add1 total-matching))
                          (set! folder-matches (add1 folder-matches))
                          (set! all-matches
                                (cons
                                 (hash 'account account
                                       'folder folder
                                       'from from
                                       'subj (main-mail-header-parts-subj hdr)
                                       'date-string (main-mail-header-parts-date-string hdr)
                                       'epoch (main-mail-header-parts-parsed-epoch hdr)
                                       'category (known-contact-category categorized from))
                                 all-matches))))))

                  (eprintf "~a matched~n" folder-matches)))

              ;; Render based on view mode
              (case view-mode
                [(by-sender)
                 (render-by-sender all-matches filter-label)]
                [(newest-first)
                 (render-newest-first all-matches filter-label)]
                [else
                 (render-grouped all-matches filter-label)])

              (printf "======================================================================~n")
              (printf "  Total unread across scanned digests: ~a~n" total-unread)
              (printf "  Matching unread (~a): ~a~n" filter-label total-matching)
              (printf "======================================================================~n"))))))))

(main)
