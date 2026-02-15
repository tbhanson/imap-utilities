#lang racket

(require rackunit
         rackunit/text-ui
         net/head
         "../src/parse-to-address-statistics.rkt")

(run-tests
 (test-suite
  "parse-to-address-statistics"

  (test-suite
   "empty list"
   (let ([under-test (parse-to-address-statistics '() "moi")])
     (check-equal? (under-test 'including-me) '())
     (check-equal? (under-test 'not-including-me) '())))

  (test-suite
   "list including me"
   (let ([under-test (parse-to-address-statistics (list "fred,moi,ginger") "moi")])
     (check-equal? (under-test 'including-me) (list "fred,moi,ginger"))
     (check-equal? (under-test 'not-including-me) '())))

  (test-suite
   "list not including me"
   (let ([under-test (parse-to-address-statistics (list "fred,ginger") "moi")])
     (check-equal? (under-test 'including-me) '())
     (check-equal? (under-test 'not-including-me) (list "fred,ginger"))))
  ))
