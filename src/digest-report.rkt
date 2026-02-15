#lang racket

;; Analyze a mailbox-digest and produce statistics:
;; - total message count
;; - top senders by message count
;; - message counts by year
;; - classification of senders as known/unknown contacts

(require
  "mailbox-digest.rkt"
  "main-mail-header-parts.rkt"
  "mail-digest.rkt"
  "parse-mail-dates.rkt"
  "known-contacts.rkt"
  gregor)

(provide
 (contract-out
  [digest-report (-> mailbox-digest? set? void?)]))


;; ---- internal helpers ----

;; Build a hash: from-address -> count
(define (count-by-sender mbd)
  (for/fold ([counts (hash)])
            ([hdr (mailbox-digest-mail-headers mbd)])
    (let ([md (mail-digest-from-header-parts hdr)])
      (values (hash-update counts (md 'from-addr) add1 0)))))

;; Build a hash: year -> count (skipping unparseable dates)
(define (count-by-year mbd)
  (for/fold ([counts (hash)])
            ([hdr (mailbox-digest-mail-headers mbd)])
    (let ([md (mail-digest-from-header-parts hdr)])
      (let ([yr (md 'year)])
        (if yr
            (values (hash-update counts yr add1 0))
            (values counts))))))

;; Sort a hash by value descending, return list of (key . value) pairs
(define (hash->sorted-pairs h #:descending? [desc? #t])
  (sort (hash->list h)
        (if desc? > <)
        #:key cdr))

;; ---- formatting helpers ----

(define (print-separator [char #\-] [width 60])
  (printf "~a~n" (make-string width char)))

(define (print-heading text)
  (newline)
  (print-separator)
  (printf "  ~a~n" text)
  (print-separator))


;; ---- main report ----

(define (digest-report mbd known-set)
  (let ([sender-counts (count-by-sender mbd)]
        [year-counts (count-by-year mbd)]
        [total (mailbox-digest-count mbd)])

    ;; Header
    (print-separator #\= 60)
    (printf "  Mail Digest Report~n")
    (printf "  Account:  ~a~n" (mailbox-digest-mail-address mbd))
    (printf "  Folder:   ~a~n" (mailbox-digest-folder-name mbd))
    (printf "  Messages: ~a~n" total)
    (printf "  Fetched:  ~a~n" (~t (mailbox-digest-timestamp mbd) "yyyy-MM-dd HH:mm:ss"))
    (print-separator #\= 60)

    ;; By year
    (print-heading "Messages by year")
    (let ([sorted-years (sort (hash->list year-counts) < #:key car)])
      (for ([pair sorted-years])
        (printf "  ~a: ~a~n" (car pair) (cdr pair))))

    ;; Top senders
    (let ([sorted-senders (hash->sorted-pairs sender-counts)]
          [unique-count (hash-count sender-counts)])
      (print-heading (format "Top senders (~a unique)" unique-count))
      (let ([top-n (take sorted-senders (min 30 (length sorted-senders)))])
        (for ([pair top-n]
              [rank (in-naturals 1)])
          (let* ([addr (car pair)]
                 [count (cdr pair)]
                 [known? (known-contact? known-set addr)]
                 [marker (if known? " *" "")])
            (printf "  ~a. ~a (~a)~a~n" rank addr count marker))))
      (when (> unique-count 30)
        (printf "  ... and ~a more~n" (- unique-count 30))))

    ;; Known vs unknown breakdown
    (when (not (set-empty? known-set))
      (let-values
          ([(known-count unknown-count)
            (for/fold ([kc 0] [uc 0])
                      ([hdr (mailbox-digest-mail-headers mbd)])
              (let* ([md (mail-digest-from-header-parts hdr)]
                     [addr (md 'from-addr)])
                (if (known-contact? known-set addr)
                    (values (add1 kc) uc)
                    (values kc (add1 uc)))))])
        (print-heading "Known vs unknown contacts")
        (printf "  From known contacts:   ~a messages~n" known-count)
        (printf "  From unknown senders:  ~a messages~n" unknown-count)

        ;; Show known contacts who actually sent mail
        (let ([known-who-sent
               (for/list ([pair (hash->sorted-pairs sender-counts)]
                          #:when (known-contact? known-set (car pair)))
                 pair)])
          (when (not (null? known-who-sent))
            (print-heading "Messages from known contacts")
            (for ([pair known-who-sent])
              (printf "  ~a (~a)~n" (car pair) (cdr pair)))))))

    (newline)
    (printf "  (* = known contact)~n")
    (print-separator #\= 60)
    (newline)))
