#lang racket

(require rackunit
         rackunit/text-ui
         net/head
         "../src/parse-from-address-statistics.rkt")

(run-tests
 (test-suite
  "parse-from-address-statistics"
  
  (check-equal? (parse-from-address-statistics '()) (hash))
   
  (let ([not-really-an-address "no at sign"])
    (check-equal?
     (parse-from-address-statistics (list not-really-an-address))
     (hash not-really-an-address 1)))

  (test-suite
   "extract-addresses assumptions"
   (check-equal?
    (extract-addresses "nobody at home" 'address)
    (list "nobody at home"))
   (check-equal?
    (extract-addresses "outer<inner>" 'address)
    (list "inner"))
   (check-equal?
    (extract-addresses "John Doe <doe@localhost>" 'address)
    (list "doe@localhost"))
   (check-equal?
    (extract-addresses "John Doe <doe@localhost>, Also John Doe <doe@localhost>" 'address)
    (list "doe@localhost" "doe@localhost")))
  ))
