#lang racket

;; Find messages with a specific flag across all saved digests.
;;
;; Usage:
;;   racket find-by-flag.rkt '$Phishing'
;;   racket find-by-flag.rkt '\Flagged'
;;   racket find-by-flag.rkt '\Flagged' --show
;;   racket find-by-flag.rkt              (no flag: list all flags found)
;;
;; Scans local digests only (no server connection). Use view-mail.rkt
;; to inspect or delete the messages on the server.
;;
;; --show    Display sender, subject, and date for each matching message

(require
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  "src/known-contacts.rkt"
  gregor)

;; ---- loading ----

(define (load-all-latest-digests)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (let ([by-key (make-hash)])
          (for ([f (directory-list dir #:build? #t)]
                #:when (regexp-match? #rx"\\.ser$" (path->string f)))
            (with-handlers ([exn:fail?
                             (lambda (e)
                               (printf "Warning: could not read ~a~n"
                                       (file-name-from-path f)))])
              (let ([mbd (load-mailbox-digest-from-file f)])
                (let ([key (cons (mailbox-digest-mail-address mbd)
                                 (mailbox-digest-folder-name mbd))])
                  (let ([existing (hash-ref by-key key #f)])
                    (when (or (not existing)
                              (datetime>? (mailbox-digest-timestamp mbd)
                                          (mailbox-digest-timestamp existing)))
                      (hash-set! by-key key mbd)))))))
          (hash-values by-key))
        '())))

;; ---- flag survey ----

(define (survey-all-flags digests)
  (let ([flag-counts (make-hash)])
    (for ([mbd digests])
      (for ([hdr (mailbox-digest-mail-headers mbd)])
        (for ([flag (main-mail-header-parts-flags hdr)])
          (hash-update! flag-counts flag add1 0))))
    (printf "Flags found across all digests:~n~n")
    (for ([pair (sort (hash->list flag-counts) > #:key cdr)])
      (printf "  ~a: ~a~n" (car pair) (cdr pair)))))

;; ---- flag search ----

(define (find-flag digests target-flag show?)
  (let ([flag-sym (string->symbol target-flag)]
        [total-matches 0]
        [categorized (load-known-contacts-categorized (default-known-contacts-filepath))])

    (for ([mbd (sort digests string<?
                     #:key (lambda (d) (format "~a/~a"
                                               (mailbox-digest-mail-address d)
                                               (mailbox-digest-folder-name d))))])
      (let ([account (mailbox-digest-mail-address mbd)]
            [folder (mailbox-digest-folder-name mbd)]
            [matches '()])

        (for ([hdr (mailbox-digest-mail-headers mbd)])
          (when (member flag-sym (main-mail-header-parts-flags hdr))
            (set! matches
                  (cons (list (main-mail-header-parts-mail-id hdr)
                              (main-mail-header-parts-from hdr)
                              (main-mail-header-parts-subj hdr)
                              (main-mail-header-parts-date-string hdr))
                        matches))))

        (when (not (null? matches))
          (set! total-matches (+ total-matches (length matches)))
          (printf "~a / ~a: ~a message(s)~n" account folder (length matches))

          (when show?
            (for ([match (reverse matches)])
              (let ([uid (first match)]
                    [from (second match)]
                    [subj (third match)]
                    [date (fourth match)]
                    [cat (known-contact-category categorized (second match))])
                (printf "  UID ~a  ~a  ~a~a~n    ~a~n"
                        uid date from
                        (if cat (format "  [~a]" cat) "")
                        (if (string=? subj "") "(no subject)" subj))))
            (newline)))))

    (printf "~n~a total message(s) with flag ~a~n" total-matches target-flag)

    (when (and (> total-matches 0) (not show?))
      (printf "~nUse --show to see sender/subject/date for each.~n")
      (printf "Use view-mail.rkt to inspect or delete on the server.~n"))))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)])
    (let ([show? (if (member "--show" arg-list) #t #f)]
          [positional (filter (lambda (a) (not (string-prefix? a "--"))) arg-list)])
      (values (if (null? positional) #f (first positional))
              show?))))

;; ---- main ----

(define (main)
  (let-values ([(target-flag show?) (parse-args (current-command-line-arguments))])

    (printf "Loading digests...~n")
    (let ([digests (load-all-latest-digests)])

      (when (null? digests)
        (printf "No digests found.~n")
        (exit 0))

      (printf "Loaded ~a digest(s).~n~n" (length digests))

      (if target-flag
          (find-flag digests target-flag show?)
          (survey-all-flags digests)))))

(main)
