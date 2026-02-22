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
  gregor)

;; ---- digest loading ----

(define (load-all-latest-digests)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (let ([by-key (make-hash)])
          (for ([f (directory-list dir #:build? #t)]
                #:when (regexp-match? #rx"\\.ser$" (path->string f)))
            (with-handlers ([exn:fail? (lambda (e) (void))])
              (let* ([mbd (load-mailbox-digest-from-file f)]
                     [key (cons (mailbox-digest-mail-address mbd)
                                (mailbox-digest-folder-name mbd))])
                (let ([existing (hash-ref by-key key #f)])
                  (when (or (not existing)
                            (datetime>? (mailbox-digest-timestamp mbd)
                                        (mailbox-digest-timestamp existing)))
                    (hash-set! by-key key mbd))))))
          (hash-values by-key))
        '())))

;; ---- counting ----

(define (count-by-year digests)
  (let ([year-counts (make-hash)]
        [year-sizes (make-hash)]
        [total 0]
        [total-size 0]
        [no-year 0]
        [has-sizes? #f])
    (for ([mbd digests])
      (for ([hdr (mailbox-digest-mail-headers mbd)])
        (set! total (add1 total))
        (let ([yr (main-mail-header-parts-parsed-year hdr)]
              [sz (main-mail-header-parts-message-size hdr)])
          (when sz
            (set! has-sizes? #t)
            (set! total-size (+ total-size sz)))
          (if yr
              (begin
                (hash-update! year-counts yr add1 0)
                (when sz
                  (hash-update! year-sizes yr (lambda (v) (+ v sz)) 0)))
              (set! no-year (add1 no-year))))))
    (values year-counts year-sizes total total-size no-year has-sizes?)))

(define (count-by-year-per-account digests)
  (let ([account-years (make-hash)]
        [account-sizes (make-hash)])
    (for ([mbd digests])
      (let ([email (mailbox-digest-mail-address mbd)])
        (for ([hdr (mailbox-digest-mail-headers mbd)])
          (let ([yr (main-mail-header-parts-parsed-year hdr)]
                [sz (main-mail-header-parts-message-size hdr)])
            (when yr
              (hash-update!
               account-years email
               (lambda (yh) (hash-update! yh yr add1 0) yh)
               (lambda () (make-hash)))
              (when sz
                (hash-update!
                 account-sizes email
                 (lambda (yh) (hash-update! yh yr (lambda (v) (+ v sz)) 0) yh)
                 (lambda () (make-hash)))))))))
    (values account-years account-sizes)))

;; ---- formatting ----

(define (format-size bytes)
  (cond
    [(>= bytes (* 1024 1024 1024)) (format "~a GB" (~r (/ bytes 1024.0 1024.0 1024.0) #:precision '(= 2)))]
    [(>= bytes (* 1024 1024))      (format "~a MB" (~r (/ bytes 1024.0 1024.0) #:precision '(= 1)))]
    [(>= bytes 1024)               (format "~a KB" (~r (/ bytes 1024.0) #:precision '(= 0)))]
    [else                           (format "~a B" bytes)]))

;; ---- display ----

(define (print-year-table year-counts year-sizes total total-size no-year has-sizes?
                          #:title [title "All Accounts Combined"]
                          #:bars [bars 'count])
  (let* ([sorted (sort (hash->list year-counts) < #:key car)]
         [max-val (if (null? sorted) 0
                      (if (and has-sizes? (eq? bars 'size))
                          (apply max (map (lambda (p) (hash-ref year-sizes (car p) 0)) sorted))
                          (apply max (map cdr sorted))))]
         [bar-width 30])

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
                          (max 1 (round (* bar-width (/ bar-val max-val)))))])
        (if has-sizes?
            (printf "  ~a  ~a  ~a  ~a~n"
                    (~a year #:min-width 6)
                    (~a count #:min-width 10 #:align 'right)
                    (~a (format-size size) #:min-width 12 #:align 'right)
                    (make-string bar-len #\█))
            (printf "  ~a  ~a  ~a~n"
                    (~a year #:min-width 6)
                    (~a count #:min-width 10 #:align 'right)
                    (make-string bar-len #\█)))))

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
        [bars 'count])
    (let loop ([remaining arg-list])
      (cond
        [(null? remaining) (void)]
        [(string=? (car remaining) "--per-account")
         (set! per-account? #t)
         (loop (cdr remaining))]
        [(and (string=? (car remaining) "--bars")
              (not (null? (cdr remaining))))
         (set! bars (string->symbol (cadr remaining)))
         (loop (cddr remaining))]
        [else (loop (cdr remaining))]))
    (values per-account? bars)))

;; ---- main ----

(define (main)
  (let-values ([(per-account? bars) (parse-args (current-command-line-arguments))])
    (let ([digests (load-all-latest-digests)])

    (when (null? digests)
      (printf "No digests found.~n")
      (exit 0))

    ;; Always show combined summary
    (let-values ([(year-counts year-sizes total total-size no-year has-sizes?)
                  (count-by-year digests)])
      (print-year-table year-counts year-sizes total total-size no-year has-sizes?
                        #:bars bars)
      (unless has-sizes?
        (printf "~n  (Run 'racket fetch-sizes.rkt' to add size data)~n")))

    ;; Optionally show per-account breakdown
    (when per-account?
      (let-values ([(account-years account-sizes) (count-by-year-per-account digests)])
        (for ([email (sort (hash-keys account-years) string<?)])
          (let* ([yh (hash-ref account-years email)]
                 [sh (hash-ref account-sizes email (make-hash))]
                 [acct-total (for/sum ([p (hash->list yh)]) (cdr p))]
                 [acct-size (for/sum ([p (hash->list sh)]) (cdr p))]
                 [has-sz? (not (hash-empty? sh))])
            (print-year-table yh sh acct-total acct-size 0 has-sz?
                              #:title (if has-sz?
                                         (format "~a (~a messages, ~a)"
                                                 email acct-total (format-size acct-size))
                                         (format "~a (~a messages)" email acct-total))
                              #:bars bars))))))))

(main)
