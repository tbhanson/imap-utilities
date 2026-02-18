#lang racket

;; Find unread messages from known contacts across all saved digests.
;;
;; Usage:
;;   racket find-unread.rkt                    ; unread from known contacts
;;   racket find-unread.rkt --all              ; unread from anyone
;;   racket find-unread.rkt --from someone@x   ; unread from a specific sender
;;
;; Scans all saved INBOX digests (not sent-mail) and shows messages
;; that don't have the \Seen flag.

(require
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  "src/mail-digest.rkt"
  "src/known-contacts.rkt"
  gregor)

;; ---- helpers ----

(define (message-seen? hdr)
  (member '|\\Seen| (main-mail-header-parts-flags hdr)))

(define (message-from-addr hdr)
  (with-handlers ([exn:fail? (lambda (e) "")])
    ((mail-digest-from-header-parts hdr) 'from-addr)))

;; ---- loading ----

;; Load all non-sent digests (latest per account+folder)
(define (load-inbox-digests)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (let ([all-files
               (for/list ([f (directory-list dir #:build? #t)]
                          #:when (regexp-match? #rx"\\.ser$" (path->string f)))
                 f)])
          ;; Load each, skip sent-mail folders
          (let ([by-key (make-hash)])
            (for ([f all-files])
              (with-handlers ([exn:fail?
                               (lambda (e)
                                 (printf "Warning: could not read ~a~n"
                                         (file-name-from-path f)))])
                (let ([mbd (load-mailbox-digest-from-file f)])
                  ;; Skip sent-mail digests
                  (unless (regexp-match? #rx"(?i:sent|gesendet|envoy|inviati|enviados|verzonden)"
                                         (mailbox-digest-folder-name mbd))
                    (let ([key (cons (mailbox-digest-mail-address mbd)
                                     (mailbox-digest-folder-name mbd))])
                      (let ([existing (hash-ref by-key key #f)])
                        (when (or (not existing)
                                  (datetime>? (mailbox-digest-timestamp mbd)
                                              (mailbox-digest-timestamp existing)))
                          (hash-set! by-key key mbd))))))))
            (hash-values by-key)))
        '())))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)])
    (let ([show-all? (member "--all" arg-list)]
          [from-filter #f])
      (let loop ([remaining arg-list])
        (cond
          [(null? remaining) (void)]
          [(and (string=? (car remaining) "--from")
                (not (null? (cdr remaining))))
           (set! from-filter (string-downcase (cadr remaining)))
           (loop (cddr remaining))]
          [else (loop (cdr remaining))]))
      (values (if show-all? #t #f) from-filter))))

;; ---- main ----

(define (main)
  (let-values ([(show-all? from-filter) (parse-args (current-command-line-arguments))])

    (printf "Loading digests...~n")
    (let ([digests (load-inbox-digests)]
          [known-set (load-known-contacts (default-known-contacts-filepath))])

      (when (null? digests)
        (printf "No digests found.~n")
        (exit 0))

      (when (and (not show-all?) (not from-filter) (set-empty? known-set))
        (printf "No known-contacts file found. Use --all to see all unread,~n")
        (printf "or create ~a first.~n" (default-known-contacts-filepath))
        (exit 0))

      (printf "Scanning ~a digest(s) for unread messages...~n~n" (length digests))

      (let ([total-unread 0]
            [total-matching 0])

        (for ([mbd (sort digests string<?
                         #:key (lambda (d) (format "~a/~a"
                                                   (mailbox-digest-mail-address d)
                                                   (mailbox-digest-folder-name d))))])
          (let ([account (mailbox-digest-mail-address mbd)]
                [folder (mailbox-digest-folder-name mbd)]
                [unread-matches '()])

            (for ([hdr (mailbox-digest-mail-headers mbd)])
              (unless (message-seen? hdr)
                (set! total-unread (add1 total-unread))
                (let ([from (message-from-addr hdr)])
                  (when (cond
                          [from-filter (string=? (string-downcase from) from-filter)]
                          [show-all? #t]
                          [else (known-contact? known-set from)])
                    (set! total-matching (add1 total-matching))
                    (set! unread-matches
                          (cons (list from
                                      (main-mail-header-parts-subj hdr)
                                      (main-mail-header-parts-date-string hdr))
                                unread-matches))))))

            (when (not (null? unread-matches))
              (printf "~a / ~a (~a unread from ~a):~n"
                      account folder
                      (length unread-matches)
                      (cond [from-filter from-filter]
                            [show-all? "anyone"]
                            [else "known contacts"]))
              ;; Show most recent first
              (for ([match (reverse unread-matches)])
                (let ([from (first match)]
                      [subj (second match)]
                      [date (third match)])
                  (printf "  ~a  ~a~n    ~a~n"
                          date from
                          (if (string=? subj "") "(no subject)" subj))))
              (newline))))

        (printf "======================================================================~n")
        (printf "  Total unread across all digests: ~a~n" total-unread)
        (printf "  Matching unread (~a): ~a~n"
                (cond [from-filter (format "from ~a" from-filter)]
                      [show-all? "all"]
                      [else "known contacts"])
                total-matching)
        (printf "======================================================================~n")))))

(main)
