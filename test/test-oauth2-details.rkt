#lang racket

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         "../src/oauth2-details.rkt")

(define-runtime-path test-data-dir "../test-data")

(run-tests
 (test-suite
  "oauth2-details"

  (test-suite
   "read from string"
   (let ([as-string "#s(oauth2-details \"a client id\" \"a client secret\" \"a redirect uri\")"])
     (let ([test-details (read-oauth2-details (open-input-string as-string))])
       (check-true (oauth2-details? test-details))
       (check-equal? (oauth2-details-client-id test-details) "a client id")
       (check-equal? (oauth2-details-client-secret test-details) "a client secret")
       (check-equal? (oauth2-details-redirect-uri test-details) "a redirect uri"))))

  (test-suite
   "read from file"
   (check-true
    (oauth2-details?
     (read-one-oauth2-details-from-file-named
      (build-path test-data-dir "one-test-oauth2-details.rkt"))))

  (test-suite
   "write and read back"
   (let ([test-email "fake@example.org"]
         [client-id "fake client id"]
         [client-secret "fake client secret"]
         [redirect-url "fake redirect-url"])
     (let ([write-path (oauth2-details-filepath-from-dir test-data-dir test-email)]
           [test-details (oauth2-details client-id client-secret redirect-url)])
       (check-true (string-contains? (path->string write-path) "test-data"))
       (check-true (string-suffix? (path->string write-path) test-email))

       ;; write
       (check-false (file-exists? write-path)
                    (format "test file ~a unexpectedly exists before write" write-path))
       (write-oauth2-details-to-file test-details write-path)

       ;; verify
       (check-true (file-exists? write-path))
       (check-equal? (file-or-directory-permissions write-path 'bits) #o600)
       (let ([roundtripped (read-one-oauth2-details-from-file-named write-path)])
         (check-equal? roundtripped test-details))

       ;; clean up
       (delete-file write-path))))
  )))
