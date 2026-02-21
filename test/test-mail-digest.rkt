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
       (check-equal? (under-test 'year) 2014)
       (check-equal?
        (under-test 'all-to)
        (for/fold ([so-far (set)])
                  ([next-to-part (list to cc bcc)])
          (values (set-union so-far (list->set (extract-addresses next-to-part 'address)))))))))

  (test-suite
   "mail-digest-from-header-parts with pre-computed date fields"
   ;; When parsed-year and parsed-epoch are populated, the digest
   ;; should use them rather than re-parsing the date string.
   (let* ([parts (main-mail-header-parts
                  100
                  "9 Oct 2014 18:23:20 -0000"
                  "sender@example.com"
                  "recipient@example.org"
                  "" "" "test subject"
                  '()
                  2014
                  1412878400)]
          [digest (mail-digest-from-header-parts parts)])
     (check-equal? (digest 'year) 2014)
     (check-equal? (digest 'from-addr) "sender@example.com")))

  (test-suite
   "mail-digest-from-header-parts with #f date fields falls back"
   ;; When parsed-year is #f, the digest should fall back to parsing
   ;; the date string.
   (let* ([parts (main-mail-header-parts
                  101
                  "9 Oct 2014 18:23:20 -0000"
                  "sender@example.com"
                  "recipient@example.org"
                  "" "" "test subject"
                  '()
                  #f #f)]
          [digest (mail-digest-from-header-parts parts)])
     (check-equal? (digest 'year) 2014)))

  (test-suite
   "mail-digest-from-header-parts via serialized test data"
   (let ([test-msg
          (deserialize 
           (read (open-input-string
                  (file->string (build-path test-data-dir "test-msg-header-serialized.rkt")))))])
     (let* ([parts (mail-header->main-mail-header-parts test-msg)]
            [under-test (mail-digest-from-header-parts parts)])
       (check-equal? (under-test 'year) 2014)
       (check-equal? (under-test 'from-addr)
                     "do-not-reply@inbound.readersupportednews.org")
       (check-equal? (under-test 'to-addrs)
                     (list "testuser@example.org")))))
  ))
