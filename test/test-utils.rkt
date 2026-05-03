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
  ))
