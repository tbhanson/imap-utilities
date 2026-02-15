#lang racket

;; Manages a "known contacts" whitelist — a plain text file with one
;; email address per line. Lines starting with # are comments.
;; Stored in ~/.imap_secrets/known-contacts
;;
;; Example file contents:
;;   # Family
;;   mom@example.com
;;   dad@example.com
;;   # Old college friends
;;   alice@example.org

(provide
 (contract-out
  [default-known-contacts-filepath (-> path?)]
  [load-known-contacts (-> path? set?)]
  [known-contact? (-> set? string? boolean?)]
  ))

(define (default-known-contacts-filepath)
  (build-path (find-system-path 'home-dir) ".imap_secrets" "known-contacts"))

;; Load known contacts from a file. Returns a set of lowercased email addresses.
;; If the file doesn't exist, returns an empty set.
(define (load-known-contacts filepath)
  (if (file-exists? filepath)
      (let ([lines (file->lines filepath)])
        (for/set ([line lines]
                  #:when (let ([trimmed (string-trim line)])
                           (and (not (string=? trimmed ""))
                                (not (string-prefix? trimmed "#")))))
          (string-downcase (string-trim line))))
      (begin
        (printf "Note: no known-contacts file at ~a~n" filepath)
        (printf "  Create one with email addresses (one per line) to classify senders.~n")
        (set))))

;; Check whether an address matches any known contact (case-insensitive).
(define (known-contact? known-set addr)
  (set-member? known-set (string-downcase addr)))
