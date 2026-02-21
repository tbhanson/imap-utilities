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
            (main-mail-header-parts id date-string from to cc bcc subj flags 2014 1412878400)])
       (check-true  (main-mail-header-parts? under-test))
       (check-equal? (main-mail-header-parts-mail-id under-test) id)
       (check-equal? (main-mail-header-parts-date-string under-test) date-string)
       (check-equal? (main-mail-header-parts-from under-test) from)
       (check-equal? (main-mail-header-parts-to under-test) to)
       (check-equal? (main-mail-header-parts-cc under-test) cc)
       (check-equal? (main-mail-header-parts-bcc under-test) bcc)
       (check-equal? (main-mail-header-parts-subj under-test) subj)
       (check-equal? (main-mail-header-parts-flags under-test) flags)
       (check-equal? (main-mail-header-parts-parsed-year under-test) 2014)
       (check-equal? (main-mail-header-parts-parsed-epoch under-test) 1412878400))))

  (test-suite
   "struct with #f date fields"
   (let ([under-test
          (main-mail-header-parts 456 "" "a@b.com" "c@d.com" "" "" "test" '() #f #f)])
     (check-true (main-mail-header-parts? under-test))
     (check-false (main-mail-header-parts-parsed-year under-test))
     (check-false (main-mail-header-parts-parsed-epoch under-test))))
  
  (test-suite
   "constants"
   (check-false (null? (member #"bcc" main-mail-header-part-labels)))
   (check-true (not (null? (member #"subject" main-mail-header-part-labels))) "should use #\"subject\" not #\"subj\"")
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
       (check-true (main-mail-header-parts? parts))
       ;; The test message is dated "9 Oct 2014 18:23:20 -0000"
       (check-equal? (main-mail-header-parts-parsed-year parts) 2014)
       (check-true (integer? (main-mail-header-parts-parsed-epoch parts)))
       ;; Subject should be populated (uses #"subject" header)
       (check-true (not (string=? "" (main-mail-header-parts-subj parts)))))))

  (test-suite
   "parsed date fields from various date formats"

   (test-case "RFC 2822 with timezone offset"
     (let ([msg (list 1
                      #"Date: Thu, 15 Mar 2024 10:30:00 +0100\r\nFrom: a@b.com\r\nSubject: test\r\n\r\n"
                      '())])
       (let ([parts (mail-header->main-mail-header-parts msg)])
         (check-equal? (main-mail-header-parts-parsed-year parts) 2024)
         (check-true (integer? (main-mail-header-parts-parsed-epoch parts))))))

   (test-case "date without day-of-week"
     (let ([msg (list 2
                      #"Date: 9 Oct 2014 18:23:20 -0000\r\nFrom: a@b.com\r\nSubject: test\r\n\r\n"
                      '())])
       (let ([parts (mail-header->main-mail-header-parts msg)])
         (check-equal? (main-mail-header-parts-parsed-year parts) 2014)
         (check-true (integer? (main-mail-header-parts-parsed-epoch parts))))))

   (test-case "single-digit day"
     (let ([msg (list 3
                      #"Date: Mon, 5 Jan 2015 08:00:00 +0000\r\nFrom: a@b.com\r\nSubject: test\r\n\r\n"
                      '())])
       (let ([parts (mail-header->main-mail-header-parts msg)])
         (check-equal? (main-mail-header-parts-parsed-year parts) 2015))))

   (test-case "date with parenthetical timezone"
     (let ([msg (list 4
                      #"Date: Fri, 20 Jun 2020 14:00:00 -0500 (CDT)\r\nFrom: a@b.com\r\nSubject: test\r\n\r\n"
                      '())])
       (let ([parts (mail-header->main-mail-header-parts msg)])
         (check-equal? (main-mail-header-parts-parsed-year parts) 2020))))

   (test-case "unparseable date gives #f"
     (let ([msg (list 5
                      #"Date: not a real date\r\nFrom: a@b.com\r\nSubject: test\r\n\r\n"
                      '())])
       (let ([parts (mail-header->main-mail-header-parts msg)])
         (check-false (main-mail-header-parts-parsed-year parts))
         (check-false (main-mail-header-parts-parsed-epoch parts)))))

   (test-case "missing date header gives #f"
     (let ([msg (list 6
                      #"From: a@b.com\r\nSubject: test\r\n\r\n"
                      '())])
       (let ([parts (mail-header->main-mail-header-parts msg)])
         (check-false (main-mail-header-parts-parsed-year parts))
         (check-false (main-mail-header-parts-parsed-epoch parts))))))
  ))
