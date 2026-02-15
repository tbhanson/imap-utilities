#lang racket

(require rackunit
         rackunit/text-ui
         "../src/parse-date-time-string-statistics.rkt")

(run-tests
 (test-suite
  "parse-date-time-string-statistics"
  
  (test-suite
   "group-date-time-strings-by-parsing-pattern"
  
   (check-equal?
    (group-date-time-strings-by-parsing-pattern '())
    (hash))
  
   (let ([non-parsable "4th of Julie"])
     (check-equal?
      (group-date-time-strings-by-parsing-pattern (list non-parsable))
      (hash #f (list non-parsable))))
  
   (let ([parsable "2015-03-15T02:02:02-04:00"])
     (check-equal?
      (group-date-time-strings-by-parsing-pattern (list parsable))
      (hash "yyyy-MM-dd'T'HH:mm:ssxxx" (list parsable)))))
 
  (test-suite
   "parse-date-time-string-statistics dispatch"
  
   (let ([under-test (parse-date-time-string-statistics '())])
     (check-equal? (under-test 'counts-by-date-string-pattern) (hash)))

   (let* ([non-parsable "4th of Julie"]
          [under-test (parse-date-time-string-statistics (list non-parsable))])
     (check-equal? (under-test 'counts-by-date-string-pattern) (hash #f 1))
     (check-equal? (under-test 'date-strings-not-parsed) (list non-parsable)))

   (let* ([parsable "2015-03-15T02:02:02-04:00"]
          [under-test (parse-date-time-string-statistics (list parsable))])
     (check-equal? (under-test 'counts-by-date-string-pattern)
                   (hash "yyyy-MM-dd'T'HH:mm:ssxxx" 1))
     (check-equal? (under-test 'counts-by-year) (hash 2015 1))
     (check-equal? (under-test 'date-strings-not-parsed) '())))
  ))
