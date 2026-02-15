#lang racket

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         gregor
         net/head
         net/imap
         racket/serialize
         "../src/mail-digest.rkt"
         "../src/main-mail-header-parts.rkt")

(define-runtime-path test-data-dir "../test-data")

(run-tests
 (test-suite
  "mail-digest"
   
  (test-suite
   "mail-digest-from-fields"
   (let ([id 123]
         [date-string "9 Oct 2014 18:23:20 -0000"]
         [from "do-not-reply@inbound.readersupportednews.org"]
         [to "testuser@example.org,other@example.net"]
         [cc "group@lists.example.com"]
         [bcc ""]
         [subj "some subject"]
         [flags (map symbol->imap-flag (list 'seen 'answered))])
     (let ([under-test
            (mail-digest-from-fields id date-string from to cc bcc subj flags)])
       (check-equal? (under-test 'date) (datetime 2014 10 9 18 23 20))
       (check-equal?
        (under-test 'all-to)
        (for/fold ([so-far (set)])
                  ([next-to-part (list to cc bcc)])
          (values (set-union so-far (list->set (extract-addresses next-to-part 'address)))))))))

  (test-suite
   "mail-digest-from-header-parts via serialized test data"
   (let ([test-msg
          (deserialize 
           (read (open-input-string
                  (file->string (build-path test-data-dir "test-msg-header-serialized.rkt")))))])
     (let* ([parts (mail-header->main-mail-header-parts test-msg)]
            [under-test (mail-digest-from-header-parts parts)])
       (check-equal? (under-test 'date) (datetime 2014 10 9 18 23 20))
       (check-equal? (under-test 'year) 2014)
       (check-equal? (under-test 'from-addr)
                     "do-not-reply@inbound.readersupportednews.org")
       (check-equal? (under-test 'to-addrs)
                     (list "testuser@example.org")))))
  ))
