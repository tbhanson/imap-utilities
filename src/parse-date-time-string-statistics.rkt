#lang racket

(provide
 group-date-time-strings-by-parsing-pattern
 parse-date-time-string-statistics
 )

(require
  gregor
  "parse-mail-dates.rkt")

;; Classify date-time strings by which parsing pattern (if any) successfully parses them.
;; Returns a hash: pattern-string -> (list-of date-time-strings)
(define (group-date-time-strings-by-parsing-pattern enumerable-of-date-time-strings)
  (for/fold ([by-pattern (hash)]
             #:result by-pattern)
            ([dts enumerable-of-date-time-strings])
    (let ([key (supported-pattern-which-parses-date-time-string? dts)])
      (values (hash-update by-pattern key
                           (lambda (so-far) (cons dts so-far)) '())))))


;; Build a statistics object (message-passing style) from a collection of date-time strings.
;; Supports queries: 'counts-by-date-string-pattern, 'show-counts-by-date-string-pattern,
;;                   'counts-by-year, 'show-counts-by-year, 'date-strings-not-parsed
(define (parse-date-time-string-statistics enumerable-of-date-time-strings)
  (define (get-counts-by-key a-hash)
    (for/fold ([counts (hash)])
              ([key (hash-keys a-hash)])
      (values (hash-set counts key (length (hash-ref a-hash key))))))
  
  (let ([by-pattern
         (group-date-time-strings-by-parsing-pattern enumerable-of-date-time-strings)]
        [counts-by-year
         (for/fold ([by-year (hash)])
                   ([dts enumerable-of-date-time-strings])
           (let ([dt (possible-parse-date-time-string dts)])
             (if dt
                 (values (hash-update by-year (->year dt) add1 0))
                 (values by-year))))])
    
    (define (dispatch msg)
      (cond
        [(eq? msg 'counts-by-date-string-pattern)
         (get-counts-by-key by-pattern)]
        [(eq? msg 'show-counts-by-date-string-pattern)
         (pretty-format (get-counts-by-key by-pattern))]
        [(eq? msg 'counts-by-year)
         counts-by-year]
        [(eq? msg 'show-counts-by-year)
         (pretty-format counts-by-year)]
        [(eq? msg 'date-strings-not-parsed)
         (hash-ref by-pattern #f '())]
        [else (error 'parse-date-time-string-statistics "unknown query: ~a" msg)]))
    dispatch))
