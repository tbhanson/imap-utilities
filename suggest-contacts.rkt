#lang racket

;; Analyze sent-mail digests to suggest known contacts.
;;
;; Usage:
;;   racket suggest-contacts.rkt
;;   racket suggest-contacts.rkt --min 3
;;   racket suggest-contacts.rkt --save
;;   racket suggest-contacts.rkt --min 10 --bare >> ~/.imap_secrets/known-contacts
;;
;; This scans all saved digests whose folder name contains "Sent"
;; (or "Gesendet", "Envoy", etc.) and extracts the To/CC addresses
;; — people YOU have written to.
;;
;; Options:
;;   --min N    Only show addresses you've written to at least N times (default: 2)
;;   --save     Append new addresses to your known-contacts file
;;   --bare     Output just the email addresses, one per line (no decoration).
;;              Useful for piping into known-contacts:
;;                racket suggest-contacts.rkt --min 10 --bare >> ~/.imap_secrets/known-contacts
;;
;; The idea: if you've written to someone multiple times, they're
;; probably a real person you care about.

(require
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  "src/mail-digest.rkt"
  "src/known-contacts.rkt"
  racket/date
  net/head)

;; ---- helpers ----

;; Safe address extraction (handles malformed headers)
(define (safe-extract-addresses addr-string)
  (with-handlers ([exn:fail? (lambda (e) '())])
    (let ([result (extract-addresses addr-string 'address)])
      (if (null? result) '() result))))

;; Find all digest files whose folder name matches sent-mail patterns
(define (find-sent-mail-digests-quiet quiet?)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (let ([all-files
               (for/list ([f (directory-list dir #:build? #t)]
                          #:when (regexp-match? #rx"\\.ser$" (path->string f)))
                 f)])
          (for/fold ([result '()])
                    ([f all-files])
            (let ([mbd (load-mailbox-digest-from-file f)])
              (if (regexp-match? #rx"(?i:sent|gesendet|envoy|inviati|enviados|verzonden)"
                                 (mailbox-digest-folder-name mbd))
                  (begin
                    (unless quiet?
                      (printf "  Found sent-mail digest: ~a / ~a (~a messages)~n"
                              (mailbox-digest-mail-address mbd)
                              (mailbox-digest-folder-name mbd)
                              (mailbox-digest-count mbd)))
                    (values (cons mbd result)))
                  (values result)))))
        '())))

;; Extract all To/CC addresses from sent-mail digests.
;; Returns hash: address -> count (number of messages sent to that address)
(define (count-recipients digests my-addresses)
  (for/fold ([counts (hash)])
            ([mbd digests])
    (for/fold ([c counts])
              ([hdr (mailbox-digest-mail-headers mbd)])
      (let* ([to-addrs (safe-extract-addresses (main-mail-header-parts-to hdr))]
             [cc-addrs (safe-extract-addresses (main-mail-header-parts-cc hdr))]
             [all-addrs (append to-addrs cc-addrs)])
        ;; Count each recipient, but skip our own addresses
        (for/fold ([cc c])
                  ([addr all-addrs])
          (let ([lower (string-downcase addr)])
            (if (set-member? my-addresses lower)
                (values cc)
                (values (hash-update cc lower add1 0)))))))))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([min-count 2]
        [save? #f]
        [bare? #f])
    (let loop ([remaining (vector->list args)])
      (cond
        [(null? remaining) (values min-count save? bare?)]
        [(and (string=? (car remaining) "--min")
              (not (null? (cdr remaining))))
         (set! min-count (string->number (cadr remaining)))
         (loop (cddr remaining))]
        [(string=? (car remaining) "--save")
         (set! save? #t)
         (loop (cdr remaining))]
        [(string=? (car remaining) "--bare")
         (set! bare? #t)
         (loop (cdr remaining))]
        [else (loop (cdr remaining))]))
    (values min-count save? bare?)))

;; ---- main ----

(define (main)
  (let-values ([(min-count save? bare?) (parse-args (current-command-line-arguments))])

    ;; In bare mode, send status messages to stderr so stdout is clean
    (define (status fmt . args)
      (unless bare?
        (apply printf fmt args)))

    (status "Scanning for sent-mail digests...~n")
    (let ([sent-digests (find-sent-mail-digests-quiet bare?)])

      (when (null? sent-digests)
        (status "~nNo sent-mail digests found.~n")
        (status "Fetch your sent mail first, e.g.:~n")
        (status "  racket fetch.rkt \"my-gmail\" \"[Gmail]/Sent Mail\"~n")
        (status "  racket fetch.rkt \"my-other\" \"Sent\"~n")
        (exit 0))

      ;; Collect our own addresses so we can exclude them
      (let* ([my-addresses
              (for/set ([mbd sent-digests])
                (string-downcase (mailbox-digest-mail-address mbd)))]
             [recipient-counts (count-recipients sent-digests my-addresses)]
             [sorted (sort (hash->list recipient-counts) > #:key cdr)]
             [filtered (filter (lambda (p) (>= (cdr p) min-count)) sorted)]
             [known-set (load-known-contacts (default-known-contacts-filepath))]
             [already-known
              (filter (lambda (p) (known-contact? known-set (car p))) filtered)]
             [new-contacts
              (filter (lambda (p) (not (known-contact? known-set (car p)))) filtered)])

        (cond
          ;; --bare mode: just output addresses, one per line
          [bare?
           (printf "# written to ~a or more times~n" min-count)
           (for ([pair new-contacts])
             (printf "~a~n" (car pair)))]


          ;; Normal display mode
          [else
           (printf "~n======================================================================~n")
           (printf "  People you've written to (at least ~a times)~n" min-count)
           (printf "======================================================================~n")
           (printf "~n  Total unique recipients: ~a~n" (hash-count recipient-counts))
           (printf "  Written to >= ~a times:  ~a~n" min-count (length filtered))
           (printf "  Already in known-contacts: ~a~n" (length already-known))
           (printf "  New (not yet in known-contacts): ~a~n" (length new-contacts))

           ;; Show new contacts
           (when (not (null? new-contacts))
             (printf "~n----------------------------------------------------------------------~n")
             (printf "  Suggested new contacts (by frequency)~n")
             (printf "----------------------------------------------------------------------~n")
             (for ([pair new-contacts]
                   [rank (in-naturals 1)])
               (printf "  ~a. ~a (~a messages)~n" rank (car pair) (cdr pair))))

           ;; Show already known
           (when (not (null? already-known))
             (printf "~n----------------------------------------------------------------------~n")
             (printf "  Already known contacts you've written to~n")
             (printf "----------------------------------------------------------------------~n")
             (for ([pair already-known])
               (printf "  ~a (~a messages)~n" (car pair) (cdr pair))))

           ;; Save if requested
           (when (and save? (not (null? new-contacts)))
             (let ([filepath (default-known-contacts-filepath)])
               (printf "~nAppending ~a new addresses to ~a ...~n"
                       (length new-contacts) filepath)
               (let ([out (open-output-file filepath #:exists 'append)])
                 (fprintf out "~n# Added by suggest-contacts (~a)~n"
                          (date->string (seconds->date (current-seconds))))
                 (for ([pair new-contacts])
                   (fprintf out "~a~n" (car pair)))
                 (close-output-port out))
               (printf "Done! Review and edit the file to remove any you don't want.~n")))

           (when (and (not save?) (not (null? new-contacts)))
             (printf "~nTo add these to your known-contacts file, run:~n")
             (printf "  racket suggest-contacts.rkt --save~n")
             (printf "  racket suggest-contacts.rkt --min ~a --bare >> ~~/.imap_secrets/known-contacts~n"
                     min-count))

           (newline)])))))

(main)
