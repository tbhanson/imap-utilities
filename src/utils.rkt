#lang racket

;; Shared utilities for the imap-utilities project.
;;
;; This module collects helper functions that were duplicated across
;; multiple tools. It contains no IO-driven logic (no IMAP, no OAuth2),
;; only pure helpers and path/file conventions.

(require racket/format
         racket/contract
         racket/file
         racket/string
         racket/path)

;; ---- standard paths ----
;;
;; Some path helpers already live in their respective modules and
;; are not (re-)exported here:
;;   - default-credentials-filepath : imap-email-account-credentials.rkt
;;   - default-secrets-dir          : imap-email-account-credentials.rkt
;;   - default-known-contacts-filepath : known-contacts.rkt
;;   - default-digest-dir           : mailbox-digest.rkt
;;
;; This module only adds path helpers that were NOT already provided.
;; We use a local secrets-dir helper to avoid the import dependency
;; while still anchoring our paths in the same .imap_secrets directory.

(define (-secrets-dir)
  (build-path (find-system-path 'home-dir) ".imap_secrets"))

(define (default-derived-contacts-filepath)
  (build-path (-secrets-dir) "derived-contacts.txt"))

(define (default-status-visits-filepath)
  (build-path (-secrets-dir) "status-visits.txt"))

;; ---- size formatting ----

;; Format a byte count for display. Handles #f and 0 by returning "-".
;; Picks the largest unit that yields a value >= 1.
;;
;; Examples:
;;   (format-size #f)              => "-"
;;   (format-size 0)               => "-"
;;   (format-size 512)             => "512 B"
;;   (format-size 2048)            => "2 KB"
;;   (format-size 5242880)         => "5.0 MB"
;;   (format-size 2147483648)      => "2.00 GB"
(define (format-size bytes)
  (cond
    [(or (not bytes) (= bytes 0)) "-"]
    [(>= bytes (* 1024 1024 1024))
     (format "~a GB" (~r (/ bytes 1024.0 1024.0 1024.0) #:precision '(= 2)))]
    [(>= bytes (* 1024 1024))
     (format "~a MB" (~r (/ bytes 1024.0 1024.0) #:precision '(= 1)))]
    [(>= bytes 1024)
     (format "~a KB" (inexact->exact (round (/ bytes 1024.0))))]
    [else
     (format "~a B" bytes)]))

;; Format a kilobyte count for display. The IMAP QUOTA RFC reports
;; storage in kilobytes, so this is a convenience wrapper.
(define (format-size-kb kb)
  (format-size (and kb (* kb 1024))))

;; ---- truncation ----

;; Truncate a string to at most max-len characters, appending "..."
;; if truncation occurred. The "..." counts toward max-len.
(define (truncate-string s max-len)
  (if (<= (string-length s) max-len)
      s
      (string-append (substring s 0 (max 0 (- max-len 3))) "...")))

;; ---- I/O ----

;; Configure stdout to handle broken pipe gracefully (e.g. when piping to head).
;; Switch to line buffering and silently swallow EPIPE / "broken pipe" errors.
;; Call once near the top of a script's main module.
(define (handle-broken-pipe)
  (file-stream-buffer-mode (current-output-port) 'line)
  (uncaught-exception-handler
   (let ([prev (uncaught-exception-handler)])
     (lambda (e)
       (if (and (exn:fail? e)
                (regexp-match? #rx"[Bb]roken pipe" (exn-message e)))
           (exit 0)
           (prev e))))))

;; ---- exports ----

(provide
 (contract-out
  ;; paths
  [default-derived-contacts-filepath (-> path?)]
  [default-status-visits-filepath (-> path?)]

  ;; formatting
  [format-size (-> (or/c #f exact-nonnegative-integer?) string?)]
  [format-size-kb (-> (or/c #f exact-nonnegative-integer?) string?)]
  [truncate-string (-> string? exact-nonnegative-integer? string?)]

  ;; I/O
  [handle-broken-pipe (-> any)]))
