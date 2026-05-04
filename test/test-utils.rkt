#lang racket

(require rackunit
         rackunit/text-ui
         "../src/utils.rkt")

(run-tests
 (test-suite
  "utils"

  (test-suite
   "format-size"
   (test-case "handles #f"
     (check-equal? (format-size #f) "-"))
   (test-case "handles 0"
     (check-equal? (format-size 0) "-"))
   (test-case "bytes"
     (check-equal? (format-size 1) "1 B")
     (check-equal? (format-size 512) "512 B")
     (check-equal? (format-size 1023) "1023 B"))
   (test-case "kilobytes"
     (check-equal? (format-size 1024) "1 KB")
     (check-equal? (format-size 2048) "2 KB")
     ;; just under 1 MB
     (check-equal? (format-size (- (* 1024 1024) 1)) "1024 KB"))
   (test-case "megabytes"
     (check-equal? (format-size (* 1024 1024)) "1.0 MB")
     (check-equal? (format-size (* 5 1024 1024)) "5.0 MB")
     ;; 1.5 MB
     (check-equal? (format-size (* 3 1024 512)) "1.5 MB"))
   (test-case "gigabytes"
     (check-equal? (format-size (* 1024 1024 1024)) "1.00 GB")
     (check-equal? (format-size (* 2 1024 1024 1024)) "2.00 GB"))
   (test-case "boundary at 1 KB"
     ;; just below threshold returns bytes
     (check-equal? (format-size 1023) "1023 B")
     ;; just at threshold returns KB
     (check-equal? (format-size 1024) "1 KB"))
   (test-case "negative not allowed by contract"
     (check-exn exn:fail:contract? (lambda () (format-size -1)))))

  (test-suite
   "format-size-kb"
   (test-case "handles #f"
     (check-equal? (format-size-kb #f) "-"))
   (test-case "handles 0"
     (check-equal? (format-size-kb 0) "-"))
   (test-case "small KB"
     (check-equal? (format-size-kb 1) "1 KB")
     (check-equal? (format-size-kb 512) "512 KB"))
   (test-case "MB"
     (check-equal? (format-size-kb 1024) "1.0 MB")
     (check-equal? (format-size-kb (* 5 1024)) "5.0 MB"))
   (test-case "GB"
     (check-equal? (format-size-kb (* 1024 1024)) "1.00 GB")))

  (test-suite
   "truncate-string"
   (test-case "shorter than max stays unchanged"
     (check-equal? (truncate-string "hello" 10) "hello"))
   (test-case "exactly at max stays unchanged"
     (check-equal? (truncate-string "hello" 5) "hello"))
   (test-case "longer than max gets truncated with ellipsis"
     (check-equal? (truncate-string "hello world" 8) "hello..."))
   (test-case "max less than 3 still produces ellipsis"
     (check-equal? (truncate-string "hello" 2) "..."))
   (test-case "max of 0 yields ellipsis"
     (check-equal? (truncate-string "hello" 0) "...")))

  (test-suite
   "default paths"
   (test-case "all return path?"
     (check-true (path? (default-derived-contacts-filepath)))
     (check-true (path? (default-status-visits-filepath))))
   (test-case "derived-contacts file is in .imap_secrets"
     (check-true
      (regexp-match?
       #rx".imap_secrets"
       (path->string (default-derived-contacts-filepath))))))
  (test-suite
   "handle-broken-pipe"
   (test-case "exists and runs without error"
     ;; Just verify the function can be called without exploding.
     ;; We can't really test EPIPE behavior in a unit test.
     (check-not-exn handle-broken-pipe)))

  (test-suite
   "extract-from-addr"
   (test-case "name and address in angle brackets"
     (check-equal? (extract-from-addr "Foo Bar <foo@example.com>")
                   "foo@example.com"))
   (test-case "bare address"
     (check-equal? (extract-from-addr "foo@example.com")
                   "foo@example.com"))
   (test-case "address is lowercased"
     (check-equal? (extract-from-addr "FOO@EXAMPLE.COM")
                   "foo@example.com")
     (check-equal? (extract-from-addr "Mixed <MiXeD@Example.COM>")
                   "mixed@example.com"))
   (test-case "empty string yields empty"
     (check-equal? (extract-from-addr "") ""))
   (test-case "garbage yields empty"
     (check-equal? (extract-from-addr "no email here at all") "")))

  (test-suite
   "load-all-latest-digests"
   (test-case "returns empty list for nonexistent directory"
     (check-equal?
      (load-all-latest-digests (build-path "/" "nonexistent" "digest" "dir"))
      '())))

  (test-suite
   "inbox-digests / sent-digests"
   ;; We can test the regexes against folder name strings without
   ;; actually constructing mailbox-digest structs by using fakes.
   ;; Instead we just exercise the regex internally.
   (test-case "INBOX matches inbox regex"
     (check-true (regexp-match?
                  #rx"(?i:^inbox$)" "INBOX")))
   (test-case "INBOX.Sent does not match inbox regex"
     (check-false (regexp-match?
                   #rx"(?i:^inbox$)" "INBOX.Sent")))
   (test-case "Various sent folders match sent regex"
     (let ([rx #rx"(?i:sent|gesendet|envoy|inviati|enviados|verzonden)"])
       (check-true (regexp-match? rx "Sent"))
       (check-true (regexp-match? rx "INBOX.Sent"))
       (check-true (regexp-match? rx "[Gmail]/Gesendet"))
       (check-true (regexp-match? rx "[Google Mail]/Gesendet"))
       (check-true (regexp-match? rx "Sent Messages"))
       (check-false (regexp-match? rx "INBOX"))
       (check-false (regexp-match? rx "Drafts"))
       (check-false (regexp-match? rx "Spam")))))

  (test-suite
   "load-all-known-contacts"
   (test-case "returns a set"
     ;; We can't predict what's in the user's actual contacts file,
     ;; but we can verify the result is a set-shaped value.
     (check-true (set? (load-all-known-contacts)))))
  ))
