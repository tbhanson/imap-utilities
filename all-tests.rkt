#lang racket

;; Run all offline (unit) tests.
;; Tests that require a live IMAP connection are NOT included here;
;; run those manually when needed.

(require
  "test/test-imap-email-account-credentials.rkt"
  "test/test-oauth2-details.rkt"
  "test/test-main-mail-header-parts.rkt"
  "test/test-mail-digest.rkt"
  "test/test-parse-mail-dates.rkt"
  "test/test-parse-date-time-string-statistics.rkt"
  "test/test-parse-from-address-statistics.rkt"
  "test/test-parse-to-address-statistics.rkt"
  )
