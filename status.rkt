#lang racket

;; Dashboard summary for periodic mail hygiene check-ins.
;;
;; Usage:
;;   racket status.rkt              ; full status, record this visit
;;   racket status.rkt --no-record  ; status without recording the visit
;;   racket status.rkt --history    ; show visit history
;;
;; What it shows:
;;   - Quota usage per account (warns at 75% / 90%)
;;   - Activity since last visit (new messages, new bulk senders)
;;   - Lifetime totals: active vs purged messages per account
;;   - Watch list: high-volume unknown senders worth attention

(require
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  "src/known-contacts.rkt"
  "src/utils.rkt"
  net/head
  gregor)

(handle-broken-pipe)

;; ---- paths ----

;; Visit history is status-specific; not in utils since no other tool uses it.
(define (default-visits-filepath) (default-status-visits-filepath))

;; ---- visit history ----

(define MAX-VISITS 500)

;; Format: 2026-05-02_14:16:55UTC (one per line, newest first)
(define visit-format "yyyy-MM-dd_HH:mm:ss'UTC'")

(define (now-utc-string)
  (~t (now/utc) visit-format))

(define (parse-visit-string s)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (parse-datetime s visit-format)))

(define (load-visits)
  (let ([path (default-visits-filepath)])
    (if (file-exists? path)
        (filter values
                (map (lambda (line)
                       (let ([trimmed (string-trim line)])
                         (and (not (string=? trimmed ""))
                              (not (regexp-match? #rx"^#" trimmed))
                              trimmed)))
                     (file->lines path)))
        '())))

(define (record-visit!)
  (let* ([path (default-visits-filepath)]
         [existing (load-visits)]
         [new-list (cons (now-utc-string)
                         (take existing (min (length existing) (- MAX-VISITS 1))))])
    (call-with-output-file path
      (lambda (out)
        (fprintf out "# Status visits (newest first)~n")
        (for ([line new-list])
          (fprintf out "~a~n" line)))
      #:exists 'replace)
    (car new-list)))

(define (last-visit-datetime)
  (let ([visits (load-visits)])
    (and (not (null? visits))
         (parse-visit-string (car visits)))))

;; ---- digest loading ----

;; Group digests by account email
(define (digests-by-account digests)
  (let ([h (make-hash)])
    (for ([mbd digests])
      (let ([email (mailbox-digest-mail-address mbd)])
        (hash-update! h email (lambda (lst) (cons mbd lst)) '())))
    h))

;; The inbox regex used in compute-account-stats below
(define inbox-rx #rx"(?i:^inbox$)")

;; ---- per-account summary ----

(struct account-stats
  (email
   active-count      ; non-tombstoned messages
   active-size
   purged-count      ; tombstoned messages
   purged-size
   inbox-count
   total-folders))

(define (compute-account-stats digests)
  (let ([by-account (digests-by-account digests)]
        [results '()])
    (for ([(email mbds) (in-hash by-account)])
      (let ([active 0] [active-sz 0]
            [purged 0] [purged-sz 0]
            [inbox-c 0])
        (for ([mbd mbds])
          (let ([is-inbox? (regexp-match? inbox-rx (mailbox-digest-folder-name mbd))])
            (for ([hdr (mailbox-digest-mail-headers mbd)])
              (let ([sz (or (main-mail-header-parts-message-size hdr) 0)])
                (if (main-mail-header-parts-deleted? hdr)
                    (begin (set! purged (+ purged 1))
                           (set! purged-sz (+ purged-sz sz)))
                    (begin (set! active (+ active 1))
                           (set! active-sz (+ active-sz sz))
                           (when is-inbox?
                             (set! inbox-c (+ inbox-c 1)))))))))
        (set! results
              (cons (account-stats email active active-sz
                                   purged purged-sz inbox-c (length mbds))
                    results))))
    (sort results string<? #:key account-stats-email)))

;; ---- since-last-visit activity ----

(define (compute-activity-since digests since-dt)
  (and since-dt
       (let ([by-account (make-hash)]
             [since-epoch (->posix since-dt)])
         (for ([mbd (inbox-digests digests)])
           (let ([email (mailbox-digest-mail-address mbd)])
             (for ([hdr (mailbox-digest-mail-headers mbd)])
               (let ([epoch (main-mail-header-parts-parsed-epoch hdr)]
                     [sz (or (main-mail-header-parts-message-size hdr) 0)])
                 (when (and epoch (>= epoch since-epoch)
                            (not (main-mail-header-parts-deleted? hdr)))
                   (hash-update! by-account email
                                 (lambda (p) (cons (+ (car p) 1) (+ (cdr p) sz)))
                                 (cons 0 0)))))))
         by-account)))

;; ---- watch list: bulk senders to consider ----

(define (compute-watch-list digests known-set since-dt)
  (let ([sender-counts (make-hash)]
        [sender-sizes (make-hash)]
        [since-epoch (and since-dt (->posix since-dt))])
    (for ([mbd (inbox-digests digests)])
      (for ([hdr (mailbox-digest-mail-headers mbd)])
        (unless (main-mail-header-parts-deleted? hdr)
          (let ([epoch (main-mail-header-parts-parsed-epoch hdr)])
            (when (or (not since-epoch)
                      (and epoch (>= epoch since-epoch)))
              (let ([from (extract-from-addr (main-mail-header-parts-from hdr))]
                    [sz (or (main-mail-header-parts-message-size hdr) 0)])
                (unless (or (string=? from "")
                            (set-member? known-set from))
                  (hash-update! sender-counts from add1 0)
                  (hash-update! sender-sizes from (lambda (v) (+ v sz)) 0))))))))
    (let ([pairs (sort (hash->list sender-counts) > #:key cdr)])
      (values pairs sender-sizes))))

;; ---- printing ----

(define (print-quota-section stats)
  (printf "~nLifetime mail (digested):~n")
  (printf "  ~a  ~a  ~a  ~a~n"
          (~a "Account" #:min-width 32)
          (~a "Active" #:min-width 14 #:align 'right)
          (~a "Purged" #:min-width 14 #:align 'right)
          (~a "INBOX" #:min-width 8 #:align 'right))
  (printf "  ~a  ~a  ~a  ~a~n"
          (make-string 32 #\-)
          (make-string 14 #\-)
          (make-string 14 #\-)
          (make-string 8 #\-))
  (for ([s stats])
    (printf "  ~a  ~a  ~a  ~a~n"
            (~a (account-stats-email s) #:min-width 32)
            (~a (format "~a (~a)"
                        (account-stats-active-count s)
                        (format-size (account-stats-active-size s)))
                #:min-width 14 #:align 'right)
            (~a (if (= (account-stats-purged-count s) 0)
                    "-"
                    (format "~a (~a)"
                            (account-stats-purged-count s)
                            (format-size (account-stats-purged-size s))))
                #:min-width 14 #:align 'right)
            (~a (account-stats-inbox-count s) #:min-width 8 #:align 'right))))

(define (print-activity-section activity since-dt)
  (printf "~nActivity since ~a:~n" (~t since-dt visit-format))
  (if (or (not activity) (= (hash-count activity) 0))
      (printf "  No new messages.~n")
      (for ([email (sort (hash-keys activity) string<?)])
        (let ([p (hash-ref activity email)])
          (printf "  ~a: +~a messages (+~a)~n"
                  email (car p) (format-size (cdr p)))))))

(define (print-watch-list pairs sender-sizes since-dt limit)
  (printf "~nWatch list — high-volume senders ~a:~n"
          (if since-dt
              (format "since ~a" (~t since-dt "yyyy-MM-dd"))
              "(all time)"))
  (if (null? pairs)
      (printf "  No bulk senders identified.~n")
      (let ([top (take pairs (min limit (length pairs)))])
        (for ([pair top])
          (let ([sender (car pair)]
                [count (cdr pair)]
                [size (hash-ref sender-sizes (car pair) 0)])
            (printf "  ~a  ~a  ~a~n"
                    (~a count #:min-width 6 #:align 'right)
                    (~a (format-size size) #:min-width 10 #:align 'right)
                    sender)))
        (when (> (length pairs) limit)
          (printf "  ... and ~a more~n" (- (length pairs) limit)))
        (printf "~n  (See `racket purge-candidates.rkt --sort size --min N` for full report)~n"))))

(define (print-history)
  (let ([visits (load-visits)])
    (printf "~nVisit history (~a recorded):~n" (length visits))
    (cond
      [(null? visits) (printf "  No visits recorded yet.~n")]
      [else
       (for ([v (take visits (min 20 (length visits)))])
         (printf "  ~a~n" v))
       (when (> (length visits) 20)
         (printf "  ... and ~a more~n" (- (length visits) 20)))])))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)]
        [no-record? #f]
        [history? #f]
        [watch-limit 10])
    (let loop ([remaining arg-list])
      (cond
        [(null? remaining) (void)]
        [(string=? (car remaining) "--no-record")
         (set! no-record? #t)
         (loop (cdr remaining))]
        [(string=? (car remaining) "--history")
         (set! history? #t)
         (loop (cdr remaining))]
        [(and (string=? (car remaining) "--limit")
              (not (null? (cdr remaining))))
         (set! watch-limit (string->number (cadr remaining)))
         (loop (cddr remaining))]
        [else (loop (cdr remaining))]))
    (values no-record? history? watch-limit)))

;; ---- main ----

(define (main)
  (let-values ([(no-record? history? watch-limit)
                (parse-args (current-command-line-arguments))])

    (when history?
      (print-history)
      (exit 0))

    (let* ([digests (load-all-latest-digests)]
           [stats (compute-account-stats digests)]
           [last-visit (last-visit-datetime)]
           [known-set (load-all-known-contacts)])

      (printf "Mail Status — ~a" (now-utc-string))
      (when last-visit
        (let ([days-ago (/ (- (->posix (now/utc)) (->posix last-visit)) 86400.0)])
          (printf " (last visit: ~a, ~a days ago)"
                  (~t last-visit visit-format)
                  (~r days-ago #:precision '(= 1)))))
      (newline)
      (printf "~a~n" (make-string 60 #\=))

      (print-quota-section stats)

      (when last-visit
        (let ([activity (compute-activity-since digests last-visit)])
          (print-activity-section activity last-visit)))

      ;; Watch list: bulk senders since last visit, or all-time if no last visit
      (let-values ([(pairs sizes)
                    (compute-watch-list digests known-set last-visit)])
        (print-watch-list pairs sizes last-visit watch-limit))

      (printf "~nKnown/derived contacts protected: ~a addresses~n"
              (set-count known-set))

      (unless no-record?
        (let ([recorded (record-visit!)])
          (printf "~nVisit recorded: ~a~n" recorded))))))

(main)
