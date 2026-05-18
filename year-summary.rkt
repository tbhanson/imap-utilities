#lang racket

;; Summarize messages by year across all accounts.
;;
;; Usage:
;;   racket year-summary.rkt              # all accounts combined
;;   racket year-summary.rkt --per-account  # breakdown per account
;;
;; Uses parsed-year from digests for fast counting. Purely local.

(require
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  "src/utils.rkt"
  gregor)

(handle-broken-pipe)

;; ---- counting ----

(define (count-by-year digests #:include-deleted? [include-deleted? #f])
  (let ([year-counts (make-hash)]
        [year-sizes (make-hash)]
        [year-del-counts (make-hash)]
        [year-del-sizes (make-hash)]
        [total 0]
        [total-size 0]
        [total-deleted 0]
        [total-deleted-size 0]
        [no-year 0]
        [has-sizes? #f])
    (for ([mbd digests])
      (for ([hdr (mailbox-digest-mail-headers mbd)])
        (let ([yr (main-mail-header-parts-parsed-year hdr)]
              [sz (main-mail-header-parts-message-size hdr)]
              [deleted? (main-mail-header-parts-deleted? hdr)])
          (when sz (set! has-sizes? #t))
          (cond
            ;; Treat deleted as active when --include-deleted
            [(and deleted? include-deleted?)
             (set! total (add1 total))
             (when sz (set! total-size (+ total-size sz)))
             (if yr
                 (begin
                   (hash-update! year-counts yr add1 0)
                   (when sz
                     (hash-update! year-sizes yr (lambda (v) (+ v sz)) 0)))
                 (set! no-year (add1 no-year)))
             ;; Still track separately for the "excluded" summary line
             (set! total-deleted (add1 total-deleted))
             (when sz (set! total-deleted-size (+ total-deleted-size sz)))
             (when yr
               (hash-update! year-del-counts yr add1 0)
               (when sz
                 (hash-update! year-del-sizes yr (lambda (v) (+ v sz)) 0)))]
            [deleted?
             (set! total-deleted (add1 total-deleted))
             (when sz (set! total-deleted-size (+ total-deleted-size sz)))
             (when yr
               (hash-update! year-del-counts yr add1 0)
               (when sz
                 (hash-update! year-del-sizes yr (lambda (v) (+ v sz)) 0)))]
            [else
             (set! total (add1 total))
             (when sz (set! total-size (+ total-size sz)))
             (if yr
                 (begin
                   (hash-update! year-counts yr add1 0)
                   (when sz
                     (hash-update! year-sizes yr (lambda (v) (+ v sz)) 0)))
                 (set! no-year (add1 no-year)))]))))
    (values year-counts year-sizes total total-size no-year has-sizes?
            year-del-counts year-del-sizes total-deleted total-deleted-size)))

(define (count-by-year-per-account digests #:include-deleted? [include-deleted? #f])
  (let ([account-years (make-hash)]
        [account-sizes (make-hash)]
        [account-del-years (make-hash)]
        [account-del-sizes (make-hash)])
    (for ([mbd digests])
      (let ([email (mailbox-digest-mail-address mbd)])
        (for ([hdr (mailbox-digest-mail-headers mbd)])
          (let ([deleted? (main-mail-header-parts-deleted? hdr)]
                [yr (main-mail-header-parts-parsed-year hdr)]
                [sz (main-mail-header-parts-message-size hdr)])
            ;; Always track deleted-per-(account,year) so split bars can
            ;; render even when --include-deleted is set.
            (when (and deleted? yr)
              (hash-update!
               account-del-years email
               (lambda (yh) (hash-update! yh yr add1 0) yh)
               (lambda () (make-hash)))
              (when sz
                (hash-update!
                 account-del-sizes email
                 (lambda (yh) (hash-update! yh yr (lambda (v) (+ v sz)) 0) yh)
                 (lambda () (make-hash)))))
            ;; Add to active counts: always for non-deleted; for deleted
            ;; only when --include-deleted.
            (when (or include-deleted? (not deleted?))
              (when yr
                (hash-update!
                 account-years email
                 (lambda (yh) (hash-update! yh yr add1 0) yh)
                 (lambda () (make-hash)))
                (when sz
                  (hash-update!
                   account-sizes email
                   (lambda (yh) (hash-update! yh yr (lambda (v) (+ v sz)) 0) yh)
                   (lambda () (make-hash))))))))))
    (values account-years account-sizes account-del-years account-del-sizes)))

;; ---- display ----

(define (print-year-table year-counts year-sizes total total-size no-year has-sizes?
                          #:title [title "All Accounts Combined"]
                          #:bars [bars 'count]
                          #:del-counts [del-counts #f]
                          #:del-sizes [del-sizes #f])
  (let* ([sorted (sort (hash->list year-counts) < #:key car)]
         [max-val (if (null? sorted) 0
                      (if (and has-sizes? (eq? bars 'size))
                          (apply max (map (lambda (p) (hash-ref year-sizes (car p) 0)) sorted))
                          (apply max (map cdr sorted))))]
         [bar-width 30]
         [show-split? (and del-counts del-sizes)])

    (printf "~n  ~a~n" title)
    (printf "  ~a~n" (make-string (string-length title) #\=))

    (if has-sizes?
        (begin
          (printf "~n  ~a  ~a  ~a  ~a~n"
                  (~a "Year" #:min-width 6)
                  (~a "Messages" #:min-width 10 #:align 'right)
                  (~a "Size" #:min-width 12 #:align 'right)
                  "")
          (printf "  ~a  ~a  ~a  ~a~n"
                  (make-string 6 #\-)
                  (make-string 10 #\-)
                  (make-string 12 #\-)
                  (make-string bar-width #\-)))
        (begin
          (printf "~n  ~a  ~a  ~a~n"
                  (~a "Year" #:min-width 6)
                  (~a "Messages" #:min-width 10 #:align 'right)
                  "")
          (printf "  ~a  ~a  ~a~n"
                  (make-string 6 #\-)
                  (make-string 10 #\-)
                  (make-string bar-width #\-))))

    (for ([pair sorted])
      (let* ([year (car pair)]
             [count (cdr pair)]
             [size (hash-ref year-sizes year 0)]
             [bar-val (if (and has-sizes? (eq? bars 'size)) size count)]
             [bar-len (if (= max-val 0) 0
                          (max 1 (round (* bar-width (/ bar-val max-val)))))]
             [bar-str
              (cond
                [show-split?
                 ;; Compute deleted portion of bar
                 (let* ([del-count (and del-counts (hash-ref del-counts year 0))]
                        [del-size (and del-sizes (hash-ref del-sizes year 0))]
                        [del-val (if (eq? bars 'size) (or del-size 0) (or del-count 0))]
                        [del-portion (if (= bar-val 0) 0
                                         (/ del-val bar-val 1.0))]
                        [del-len (inexact->exact (round (* bar-len del-portion)))]
                        [kept-len (- bar-len del-len)])
                   (string-append (make-string kept-len #\█)
                                  (make-string del-len #\░)))]
                [else (make-string bar-len #\█)])])
        (if has-sizes?
            (printf "  ~a  ~a  ~a  ~a~n"
                    (~a year #:min-width 6)
                    (~a count #:min-width 10 #:align 'right)
                    (~a (format-size size) #:min-width 12 #:align 'right)
                    bar-str)
            (printf "  ~a  ~a  ~a~n"
                    (~a year #:min-width 6)
                    (~a count #:min-width 10 #:align 'right)
                    bar-str))))

    (printf "  ~a  ~a" (make-string 6 #\-) (make-string 10 #\-))
    (when has-sizes? (printf "  ~a" (make-string 12 #\-)))
    (newline)

    (if has-sizes?
        (printf "  ~a  ~a  ~a~n"
                (~a "Total" #:min-width 6)
                (~a total #:min-width 10 #:align 'right)
                (~a (format-size total-size) #:min-width 12 #:align 'right))
        (printf "  ~a  ~a~n"
                (~a "Total" #:min-width 6)
                (~a total #:min-width 10 #:align 'right)))
    (when (> no-year 0)
      (printf "  ~a  ~a  (no parseable date)~n"
              (~a "" #:min-width 6)
              (~a no-year #:min-width 10 #:align 'right)))))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)]
        [per-account? #f]
        [bars 'count]
        [include-deleted? #f])
    (let loop ([remaining arg-list])
      (cond
        [(null? remaining) (void)]
        [(string=? (car remaining) "--per-account")
         (set! per-account? #t)
         (loop (cdr remaining))]
        [(or (string=? (car remaining) "--include-deleted")
             (string=? (car remaining) "--all-time"))
         (set! include-deleted? #t)
         (loop (cdr remaining))]
        [(and (string=? (car remaining) "--bars")
              (not (null? (cdr remaining))))
         (set! bars (string->symbol (cadr remaining)))
         (loop (cddr remaining))]
        [else (loop (cdr remaining))]))
    (values per-account? bars include-deleted?)))

;; ---- main ----

(define (main)
  (let-values ([(per-account? bars include-deleted?)
                (parse-args (current-command-line-arguments))])
    (let ([digests (load-all-latest-digests)])

    (when (null? digests)
      (printf "No digests found.~n")
      (exit 0))

    (when include-deleted?
      (printf "(Including deleted messages in counts — full-lifetime view)~n"))

    ;; Always show combined summary
    (let-values ([(year-counts year-sizes total total-size no-year has-sizes?
                   year-del-counts year-del-sizes total-deleted total-deleted-size)
                  (count-by-year digests #:include-deleted? include-deleted?)])
      (print-year-table year-counts year-sizes total total-size no-year has-sizes?
                        #:bars bars
                        #:del-counts (and include-deleted? year-del-counts)
                        #:del-sizes (and include-deleted? year-del-sizes))
      (when (and (> total-deleted 0) (not include-deleted?))
        (printf "~n  Excluded ~a deleted messages (~a)~n"
                total-deleted (format-size total-deleted-size)))
      (when (and (> total-deleted 0) include-deleted?)
        (printf "~n  (Of those, ~a are tombstoned/deleted: ~a)~n"
                total-deleted (format-size total-deleted-size)))
      (unless has-sizes?
        (printf "~n  (Run 'racket fetch-sizes.rkt' to add size data)~n")))

    ;; Optionally show per-account breakdown
    (when per-account?
      (let-values ([(account-years account-sizes account-del-years account-del-sizes)
                    (count-by-year-per-account digests #:include-deleted? include-deleted?)])
        (for ([email (sort (hash-keys account-years) string<?)])
          (let* ([yh (hash-ref account-years email)]
                 [sh (hash-ref account-sizes email (make-hash))]
                 [dyh (hash-ref account-del-years email (make-hash))]
                 [dsh (hash-ref account-del-sizes email (make-hash))]
                 [acct-total (for/sum ([p (hash->list yh)]) (cdr p))]
                 [acct-size (for/sum ([p (hash->list sh)]) (cdr p))]
                 [has-sz? (not (hash-empty? sh))])
            (print-year-table yh sh acct-total acct-size 0 has-sz?
                              #:title (if has-sz?
                                         (format "~a (~a messages, ~a)"
                                                 email acct-total (format-size acct-size))
                                         (format "~a (~a messages)" email acct-total))
                              #:bars bars
                              #:del-counts (and include-deleted? dyh)
                              #:del-sizes (and include-deleted? dsh)))))))))

(main)
