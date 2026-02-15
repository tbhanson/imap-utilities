#lang racket

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         racket/serialize
         gregor
         "../src/digest-report.rkt"
         "../src/mailbox-digest.rkt"
         "../src/main-mail-header-parts.rkt")

(define-runtime-path test-data-dir "../test-data")

;; Build a small mailbox-digest from our test data for report testing.
(define (make-test-mailbox-digest)
  (let ([test-msg
         (deserialize 
          (read (open-input-string
                 (file->string
                  (build-path test-data-dir "test-msg-header-serialized.rkt")))))])
    (let ([hdr (mail-header->main-mail-header-parts test-msg)])
      ;; Create a digest with a few copies to make the report interesting
      (mailbox-digest
       "testuser@example.org"
       "INBOX"
       12345
       (cons 1 3)
       (list hdr hdr hdr)
       (datetime 2025 7 1 12 0 0)))))

(run-tests
 (test-suite
  "digest-report"

  (test-suite
   "report runs without error"
   ;; Just verify it doesn't crash; output goes to stdout
   (let ([mbd (make-test-mailbox-digest)])
     (check-not-exn
      (lambda ()
        (with-output-to-string
          (lambda () (digest-report mbd (set))))))
     ;; With known contacts
     (check-not-exn
      (lambda ()
        (with-output-to-string
          (lambda ()
            (digest-report mbd
                           (set "do-not-reply@inbound.readersupportednews.org"))))))))
  ))
