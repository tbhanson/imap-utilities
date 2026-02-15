#lang racket

(require rackunit
         rackunit/text-ui
         gregor
         "../src/parse-mail-dates.rkt")

(run-tests
 (test-suite
  "parse-mail-dates"

  (test-suite
   "known patterns exist"
   (check-false (empty? supported-mail-patterns)))

  (test-suite
   "parsable-as-datetime?"
   (check-true  (parsable-as-datetime? "2015-03-15T02:02:02-04:00" "yyyy-MM-dd'T'HH:mm:ssxxx"))
   (check-false (parsable-as-datetime? "21 Jun 2015 15:45:40 -0000" "yyyy-MM-dd'T'HH:mm:ssxxx")))

  (test-suite
   "supported-pattern-which-parses-date-time-string?"
   (check-equal?
    (supported-pattern-which-parses-date-time-string? "2015-03-15T02:02:02-04:00")
    "yyyy-MM-dd'T'HH:mm:ssxxx")
   (check-equal?
    (supported-pattern-which-parses-date-time-string? "21 Jun 2015 15:45:40 -0000")
    "dd MMM yyyy HH:mm:ss xxxx")
   ;; parenthetical timezone suffix
   (check-equal?
    (supported-pattern-which-parses-date-time-string? "Sun, 21 Jun 2015 17:50:44 -0500 (CDT)")
    "EEE, dd MMM yyyy HH:mm:ss xxxx")
   (check-equal?
    (supported-pattern-which-parses-date-time-string? "Thu, 9 Oct 2014 15:00:02 -0700 (PDT)")
    "EEE, d MMM yyyy HH:mm:ss xxxx")
   ;; single digit day of month
   (check-equal?
    (supported-pattern-which-parses-date-time-string? "9 Oct 2014 22:20:57 -0000")
    "d MMM yyyy HH:mm:ss xxxx")
   ;; numeric month
   (check-equal?
    (supported-pattern-which-parses-date-time-string? "Mon, 20 10 2014 04:00:10")
    "EEE, dd MM yyyy HH:mm:ss")
   ;; non-standard 4-letter day abbreviation
   (check-equal?
    (supported-pattern-which-parses-date-time-string? "Tues, 28 Oct 2014 03:42:57 -0800")
    "dd MMM yyyy HH:mm:ss xxxx"))
   
  (test-suite
   "possible-parse-date-time-string"
   (check-true (datetime? (possible-parse-date-time-string "2015-03-15T02:02:02-04:00")))
   (check-true (datetime? (possible-parse-date-time-string "Sun, 21 Jun 2015 14:38:22 +0000")))
   (check-equal?
    (possible-parse-date-time-string "2015-03-15T02:02:02-04:00")
    (datetime 2015 3 15 2 2 2))
   (check-equal?
    (possible-parse-date-time-string "21 Jun 2015 15:45:40")
    (datetime 2015 6 21 15 45 40))
   (check-equal?
    (possible-parse-date-time-string "Sun, 21 Jun 2015 15:38:28 -0400 (EDT)")
    (datetime 2015 6 21 15 38 28))
   (check-false
    (possible-parse-date-time-string "June 2, 1959; 12:34:56")))
  ))
