#lang racket

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         "../src/known-contacts.rkt")

(define-runtime-path test-data-dir "../test-data")

(run-tests
 (test-suite
  "known-contacts"

  (test-suite
   "load from file"
   ;; loading from a non-existent file gives empty set
   (check-equal?
    (load-known-contacts (build-path test-data-dir "no-such-file"))
    (set))

   ;; loading from a test file
   (let ([known (load-known-contacts
                 (build-path test-data-dir "test-known-contacts.txt"))])
     (check-true (set-member? known "alice@example.com"))
     (check-true (set-member? known "bob@example.org"))
     (check-false (set-member? known "# this is a comment"))
     (check-false (set-member? known ""))))

  (test-suite
   "known-contact? is case-insensitive"
   (let ([known (set "alice@example.com" "bob@example.org")])
     (check-true  (known-contact? known "Alice@Example.COM"))
     (check-true  (known-contact? known "bob@example.org"))
     (check-false (known-contact? known "stranger@elsewhere.net"))))
  ))
