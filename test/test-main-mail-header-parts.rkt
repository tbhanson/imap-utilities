#lang racket

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         "../src/main-mail-header-parts.rkt"
         racket/serialize
         net/imap)

(define-runtime-path test-data-dir "../test-data")

(run-tests
 (test-suite
  "main-mail-header-parts"
 
  (test-suite
   "struct basics"
   (let ([id 123]
         [date-string "9 Oct 2014 18:23:20 -0000"]
         [from "do-not-reply@inbound.readersupportednews.org"]
         [to "testuser@example.org"]
         [cc ""]
         [bcc ""]
         [subj "some subject"]
         [flags (map symbol->imap-flag (list 'seen 'answered))])
     (let ([under-test
            (main-mail-header-parts id date-string from to cc bcc subj flags)])
       (check-true  (main-mail-header-parts? under-test))
       (check-equal? (main-mail-header-parts-mail-id under-test) id)
       (check-equal? (main-mail-header-parts-date-string under-test) date-string)
       (check-equal? (main-mail-header-parts-from under-test) from)
       (check-equal? (main-mail-header-parts-to under-test) to)
       (check-equal? (main-mail-header-parts-cc under-test) cc)
       (check-equal? (main-mail-header-parts-bcc under-test) bcc)
       (check-equal? (main-mail-header-parts-subj under-test) subj)
       (check-equal? (main-mail-header-parts-flags under-test) flags))))
  
  (test-suite
   "constants"
   (check-false (null? (member #"bcc" main-mail-header-part-labels)))
   (check-equal? main-mail-header-part-imap-symbols '(uid header flags)))

  (test-suite
   "mail-header->main-mail-header-parts from serialized test data"
   (let ([test-msg
          (deserialize 
           (read (open-input-string
                  (file->string (build-path test-data-dir "test-msg-header-serialized.rkt")))))])
     (check-true (list? test-msg))
     (check-equal? (length test-msg) 3)
     (check-true (integer? (car test-msg)))
     (let ([parts (mail-header->main-mail-header-parts test-msg)])
       (check-true (main-mail-header-parts? parts)))))
  ))
