#lang racket

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         "../src/imap-email-account-credentials.rkt")

;; Resolve test-data paths relative to this file, not the working directory.
(define-runtime-path test-data-dir "../test-data")

(run-tests
 (test-suite
  "imap-email-account-credentials"
 
  (test-suite
   "default paths"
   (check-true (path? (default-credentials-filepath)))
   (check-true (path? (default-secrets-dir))))

  (test-suite
   "struct basics"
   (let ([some-accountname "my account"]
         [some-host "imap.example.org"]
         [some-address "fred@example.org"]
         [some-pwd "fred's password"]
         [try-tls? #f]
         [xoauth2? #f])
     (let ((test1 (imap-email-account-credentials some-accountname some-host some-address some-pwd try-tls? xoauth2?)))
       (check-true (imap-email-account-credentials? test1))
       (check-equal? (imap-email-account-credentials-accountname test1) some-accountname)
       (check-equal? (imap-email-account-credentials-hostname test1) some-host)
       (check-equal? (imap-email-account-credentials-mailaddress test1) some-address)
       (check-equal? (imap-email-account-credentials-password test1) some-pwd)
       (check-equal? (imap-email-account-credentials-try-tls? test1) try-tls?)
       (check-equal? (imap-email-account-credentials-xoauth2? test1) xoauth2?))))

  (test-suite
   "read one from string"
   (let ([cred-string "#s(imap-email-account-credentials \"an account\" \"imap.example.com\" \"me@example.net\" \"secret stuff\" #f #f)"])
     (let ([test2 (read-one-email-account-credential
                   (open-input-string cred-string))])
       (check-true (imap-email-account-credentials? test2))
       (check-equal? (imap-email-account-credentials-accountname test2) "an account")
       (check-equal? (imap-email-account-credentials-mailaddress test2) "me@example.net"))))

  (test-suite
   "read one from file"
   (check-true
    (imap-email-account-credentials?
     (read-one-email-account-credentials-from-file-named
      (build-path test-data-dir "one-test-imap-email-account-credentials.rkt")))))

  (test-suite
   "read hash from file"
   (let ([filename (build-path test-data-dir "list-of-test-imap-email-account-credentials.rkt")])
     ;; raw read produces a list of two credentials
     (check-equal?
      (map imap-email-account-credentials?
           (read (open-input-string (file->string filename))))
      '(#t #t))

     ;; reading into a hash, keyed by accountname
     (let ([cred-hash (read-email-account-credentials-hash-from-file-named filename)])
       (check-true  (hash-has-key? cred-hash "an account"))
       (check-false (hash-has-key? cred-hash "nonexistent")))))
  ))
