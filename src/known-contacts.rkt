#lang racket

;; Manages a "known contacts" file — a plain text file with one email
;; address per line. Lines starting with # serve double duty:
;;
;;   - Pure comments (e.g. "# this is just a note") are ignored
;;   - Category headers (e.g. "# family") assign a category to all
;;     addresses that follow, until the next category header
;;
;; A category header is any # line whose content (after #) is a
;; single word or short phrase. All addresses before the first
;; category header get the category "uncategorized".
;;
;; Example file:
;;   # family
;;   mom@example.com
;;   dad@example.com
;;   # friends
;;   alice@example.org
;;   # written to 50 or more times
;;   bob@work.com

(provide
 (contract-out
  [default-known-contacts-filepath (-> path?)]
  [load-known-contacts (-> path? set?)]
  [load-known-contacts-categorized (-> path? hash?)]
  [known-contact? (-> set? string? boolean?)]
  [known-contact-category (-> hash? string? (or/c string? #f))]
  [contacts-in-category (-> hash? string? set?)]
  [contact-categories (-> hash? (listof string?))]
  ))

(define (default-known-contacts-filepath)
  (build-path (find-system-path 'home-dir) ".imap_secrets" "known-contacts"))

;; Is this line a category header?
;; We treat any "# <text>" line as a category header.
(define (category-header? line)
  (let ([trimmed (string-trim line)])
    (and (string-prefix? trimmed "#")
         (let ([after-hash (string-trim (substring trimmed 1))])
           (and (not (string=? after-hash ""))
                after-hash)))))

;; Load known contacts with category information.
;; Returns a hash: lowercased-address -> category-string
;; Category is the text after # from the most recent category header.
(define (load-known-contacts-categorized filepath)
  (if (file-exists? filepath)
      (let ([lines (file->lines filepath)])
        (let loop ([remaining lines]
                   [current-category "uncategorized"]
                   [result (hash)])
          (if (null? remaining)
              result
              (let* ([line (car remaining)]
                     [trimmed (string-trim line)])
                (cond
                  ;; Empty line — skip
                  [(string=? trimmed "")
                   (loop (cdr remaining) current-category result)]
                  ;; Category header — update current category
                  [(category-header? trimmed)
                   (loop (cdr remaining) (category-header? trimmed) result)]
                  ;; Email address — add with current category
                  [else
                   (loop (cdr remaining)
                         current-category
                         (hash-set result
                                   (string-downcase trimmed)
                                   current-category))])))))
      (begin
        (printf "Note: no known-contacts file at ~a~n" filepath)
        (printf "  Create one with email addresses (one per line) to classify senders.~n")
        (hash))))

;; Load known contacts as a flat set (backwards compatible).
;; Returns a set of lowercased email addresses.
(define (load-known-contacts filepath)
  (let ([categorized (load-known-contacts-categorized filepath)])
    (list->set (hash-keys categorized))))

;; Check whether an address matches any known contact (case-insensitive).
(define (known-contact? known-set addr)
  (set-member? known-set (string-downcase addr)))

;; Get the category for an address, or #f if not known.
(define (known-contact-category categorized-hash addr)
  (hash-ref categorized-hash (string-downcase addr) #f))

;; Get all addresses in a given category.
(define (contacts-in-category categorized-hash category)
  (let ([cat-lower (string-downcase category)])
    (for/set ([(addr cat) (in-hash categorized-hash)]
              #:when (string=? (string-downcase cat) cat-lower))
      addr)))

;; List all unique categories present in the contacts.
(define (contact-categories categorized-hash)
  (sort (set->list
         (for/set ([cat (in-hash-values categorized-hash)])
           cat))
        string<?))
