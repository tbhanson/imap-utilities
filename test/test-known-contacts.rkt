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
   "load from file (flat set)"
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

  (test-suite
   "categorized loading"
   (let ([cats (load-known-contacts-categorized
                (build-path test-data-dir "test-known-contacts.txt"))])
     ;; alice is under "Family"
     (check-equal? (known-contact-category cats "alice@example.com") "Family")
     ;; bob is under "Friends"
     (check-equal? (known-contact-category cats "bob@example.org") "Friends")
     ;; unknown address
     (check-false (known-contact-category cats "stranger@elsewhere.net"))
     ;; case insensitive
     (check-equal? (known-contact-category cats "ALICE@EXAMPLE.COM") "Family")))

  (test-suite
   "contacts-in-category"
   (let ([cats (load-known-contacts-categorized
                (build-path test-data-dir "test-known-contacts.txt"))])
     (check-equal? (contacts-in-category cats "Family")
                   (set "alice@example.com"))
     (check-equal? (contacts-in-category cats "Friends")
                   (set "bob@example.org"))
     ;; case insensitive category matching
     (check-equal? (contacts-in-category cats "family")
                   (set "alice@example.com"))
     ;; nonexistent category
     (check-equal? (contacts-in-category cats "enemies")
                   (set))))

  (test-suite
   "contact-categories"
   (let ([cats (load-known-contacts-categorized
                (build-path test-data-dir "test-known-contacts.txt"))])
     (check-equal? (contact-categories cats)
                   (list "Family" "Friends"))))
  ))
