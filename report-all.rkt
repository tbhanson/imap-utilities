#lang racket

;; Load ALL saved digests and produce a combined statistics report.
;;
;; Usage:
;;   racket report-all.rkt
;;
;; Shows:
;; - Summary of each account/folder
;; - Combined unique senders across all accounts
;; - Top senders across all accounts (with which accounts they appear in)
;; - Messages by year across all accounts
;; - Known vs unknown breakdown (if known-contacts file exists)

(require
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  "src/mail-digest.rkt"
  "src/known-contacts.rkt"
  gregor)

;; ---- loading ----

(define (all-digest-files)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (sort
         (for/list ([f (directory-list dir #:build? #t)]
                    #:when (regexp-match? #rx"\\.ser$" (path->string f)))
           f)
         string<?
         #:key path->string)
        '())))

;; For each account+folder, keep only the most recent digest.
;; Returns a list of (path . mailbox-digest) pairs.
(define (load-latest-digests)
  (let ([files (all-digest-files)])
    (printf "Found ~a digest files. Loading...~n" (length files))
    (let ([all-digests
           (for/list ([f files])
             (printf "  ~a~n" (file-name-from-path f))
             (cons f (load-mailbox-digest-from-file f)))])
      ;; Group by (email . folder), keep the one with the latest timestamp
      (let ([by-key (make-hash)])
        (for ([pair all-digests])
          (let* ([mbd (cdr pair)]
                 [key (cons (mailbox-digest-mail-address mbd)
                            (mailbox-digest-folder-name mbd))]
                 [existing (hash-ref by-key key #f)])
            (when (or (not existing)
                      (datetime>? (mailbox-digest-timestamp mbd)
                                  (mailbox-digest-timestamp (cdr existing))))
              (hash-set! by-key key pair))))
        (map cdr (hash-values by-key))))))

;; ---- analysis helpers ----

(define (print-separator [char #\-] [width 70])
  (printf "~a~n" (make-string width char)))

(define (print-heading text)
  (newline)
  (print-separator)
  (printf "  ~a~n" text)
  (print-separator))

;; Build combined sender counts across multiple digests.
;; Returns hash: address -> (hash 'total count 'accounts (set-of account-emails))
(define (combined-sender-info digests)
  (for/fold ([result (hash)])
            ([mbd digests])
    (let ([account (mailbox-digest-mail-address mbd)])
      (for/fold ([r result])
                ([hdr (mailbox-digest-mail-headers mbd)])
        (let ([addr (with-handlers ([exn:fail? (lambda (e) #f)])
                      ((mail-digest-from-header-parts hdr) 'from-addr))])
          (if addr
              (values (hash-update r addr
                                   (lambda (info)
                                     (hash 'total (add1 (hash-ref info 'total))
                                           'accounts (set-add (hash-ref info 'accounts) account)))
                                   (hash 'total 0 'accounts (set))))
              (values r)))))))

;; Build combined year counts across multiple digests.
(define (combined-year-counts digests)
  (for/fold ([counts (hash)])
            ([mbd digests])
    (for/fold ([c counts])
              ([hdr (mailbox-digest-mail-headers mbd)])
      (let ([yr (with-handlers ([exn:fail? (lambda (e) #f)])
                  ((mail-digest-from-header-parts hdr) 'year))])
        (if yr
            (values (hash-update c yr add1 0))
            (values c))))))

;; ---- main report ----

(define (main)
  (printf "Loading all digests...~n")
  (let ([digests (load-latest-digests)]
        [known-set (load-known-contacts (default-known-contacts-filepath))])

    (when (null? digests)
      (printf "No digests found.~n")
      (exit 0))

    (let ([total-messages (for/sum ([mbd digests]) (mailbox-digest-count mbd))]
          [sender-info (combined-sender-info digests)]
          [year-counts (combined-year-counts digests)])

      ;; Header
      (print-separator #\= 70)
      (printf "  Combined Mail Report — All Accounts~n")
      (print-separator #\= 70)

      ;; Per-account summary
      (print-heading "Accounts")
      (for ([mbd (sort digests string<?
                       #:key mailbox-digest-mail-address)])
        (printf "  ~a / ~a: ~a messages~n"
                (mailbox-digest-mail-address mbd)
                (mailbox-digest-folder-name mbd)
                (mailbox-digest-count mbd)))
      (printf "~n  Total: ~a messages across ~a account(s)~n"
              total-messages (length digests))

      ;; By year
      (print-heading "Messages by year (all accounts)")
      (for ([pair (sort (hash->list year-counts) < #:key car)])
        (printf "  ~a: ~a~n" (car pair) (cdr pair)))

      ;; Top senders
      (let* ([sorted-senders
              (sort (hash->list sender-info) >
                    #:key (lambda (p) (hash-ref (cdr p) 'total)))]
             [unique-count (length sorted-senders)])

        (print-heading (format "Top senders across all accounts (~a unique)" unique-count))
        (let ([top-n (take sorted-senders (min 50 (length sorted-senders)))])
          (for ([pair top-n]
                [rank (in-naturals 1)])
            (let* ([addr (car pair)]
                   [info (cdr pair)]
                   [count (hash-ref info 'total)]
                   [accts (hash-ref info 'accounts)]
                   [known? (known-contact? known-set addr)]
                   [marker (if known? " *" "")]
                   [acct-note (if (> (set-count accts) 1)
                                  (format " [~a accounts]" (set-count accts))
                                  "")])
              (printf "  ~a. ~a (~a)~a~a~n" rank addr count marker acct-note))))
        (when (> unique-count 50)
          (printf "  ... and ~a more~n" (- unique-count 50))))

      ;; Known vs unknown
      (when (not (set-empty? known-set))
        (let-values
            ([(known-count unknown-count)
              (for/fold ([kc 0] [uc 0])
                        ([mbd digests])
                (for/fold ([k kc] [u uc])
                          ([hdr (mailbox-digest-mail-headers mbd)])
                  (let ([addr (with-handlers ([exn:fail? (lambda (e) #f)])
                                ((mail-digest-from-header-parts hdr) 'from-addr))])
                    (if (and addr (known-contact? known-set addr))
                        (values (add1 k) u)
                        (values k (add1 u))))))])
          (print-heading "Known vs unknown contacts")
          (printf "  From known contacts:   ~a messages~n" known-count)
          (printf "  From unknown senders:  ~a messages~n" unknown-count)))

      (newline)
      (printf "  (* = known contact)~n")
      (print-separator #\= 70)
      (newline))))

(main)
