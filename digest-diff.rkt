#lang racket

;; Compare two digest snapshots for the same account+folder.
;;
;; Usage:
;;   racket digest-diff.rkt                     ; diff all accounts that have 2+ digests
;;   racket digest-diff.rkt --account tbh361    ; just one account (substring match)
;;   racket digest-diff.rkt --verbose           ; show sample appeared/disappeared subjects
;;   racket digest-diff.rkt --full              ; show all appeared/disappeared messages
;;
;; Compares by UID: messages that exist in the newer digest but not the
;; older one are "appeared"; the reverse are "disappeared". Also reports
;; flag changes (e.g. messages marked read, flagged, or deleted).

(require
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  gregor)

;; ---- digest loading ----

(define (default-digest-dir)
  (build-path (find-system-path 'home-dir) ".imap_secrets" "digests"))

;; Returns hash: (email . folder) -> (list digest-path ...) sorted oldest first
(define (load-all-digests-grouped)
  (let ([dir (default-digest-dir)]
        [by-key (make-hash)])
    (when (directory-exists? dir)
      (for ([f (directory-list dir #:build? #t)]
            #:when (regexp-match? #rx"\\.ser$" (path->string f)))
        (with-handlers ([exn:fail? (lambda (e) (void))])
          (let* ([mbd (load-mailbox-digest-from-file f)]
                 [key (cons (mailbox-digest-mail-address mbd)
                            (mailbox-digest-folder-name mbd))])
            (hash-update! by-key key
                          (lambda (lst) (cons (cons f mbd) lst))
                          '())))))
    ;; Sort each group by timestamp ascending
    (for/hash ([(key entries) (in-hash by-key)])
      (values key
              (sort entries
                    (lambda (a b)
                      (datetime<? (mailbox-digest-timestamp (cdr a))
                                  (mailbox-digest-timestamp (cdr b)))))))))

;; ---- formatting ----

(define (format-size bytes)
  (cond
    [(not bytes) "-"]
    [(>= bytes (* 1024 1024 1024)) (format "~a GB" (~r (/ bytes 1024.0 1024.0 1024.0) #:precision '(= 2)))]
    [(>= bytes (* 1024 1024))      (format "~a MB" (~r (/ bytes 1024.0 1024.0) #:precision '(= 1)))]
    [(>= bytes 1024)               (format "~a KB" (~r (/ bytes 1024.0) #:precision '(= 0)))]
    [else                           (format "~a B" bytes)]))

(define (format-timestamp dt)
  (~t dt "yyyy-MM-dd HH:mm"))

(define (truncate-string s max-len)
  (if (<= (string-length s) max-len)
      s
      (string-append (substring s 0 (- max-len 3)) "...")))

;; ---- diffing ----

(struct diff-result
  (email folder
   old-timestamp new-timestamp
   old-count new-count
   appeared disappeared
   appeared-size disappeared-size
   flag-changes)
  #:transparent)

;; Compare two digests. Returns a diff-result.
(define (diff-digests old-mbd new-mbd)
  (let ([email (mailbox-digest-mail-address new-mbd)]
        [folder (mailbox-digest-folder-name new-mbd)]
        [old-by-uid (make-hash)]
        [new-by-uid (make-hash)])

    ;; Index old digest by UID
    (for ([hdr (mailbox-digest-mail-headers old-mbd)])
      (hash-set! old-by-uid (main-mail-header-parts-mail-id hdr) hdr))

    ;; Index new digest by UID
    (for ([hdr (mailbox-digest-mail-headers new-mbd)])
      (hash-set! new-by-uid (main-mail-header-parts-mail-id hdr) hdr))

    ;; Appeared: in new but not old
    (let* ([appeared
            (for/list ([(uid hdr) (in-hash new-by-uid)]
                       #:when (not (hash-has-key? old-by-uid uid)))
              hdr)]
           [disappeared
            (for/list ([(uid hdr) (in-hash old-by-uid)]
                       #:when (not (hash-has-key? new-by-uid uid)))
              hdr)]
           ;; Flag changes: same UID, different flags
           [flag-changes
            (for/fold ([changes '()])
                      ([(uid new-hdr) (in-hash new-by-uid)]
                       #:when (hash-has-key? old-by-uid uid))
              (let ([old-hdr (hash-ref old-by-uid uid)])
                (let ([old-flags (sort (map symbol->string
                                            (main-mail-header-parts-flags old-hdr))
                                       string<?)]
                      [new-flags (sort (map symbol->string
                                            (main-mail-header-parts-flags new-hdr))
                                       string<?)])
                  (if (equal? old-flags new-flags)
                      changes
                      (cons (list uid old-flags new-flags
                                  (main-mail-header-parts-subj new-hdr))
                            changes)))))]
           ;; Size totals
           [appeared-size
            (for/sum ([hdr appeared])
              (or (main-mail-header-parts-message-size hdr) 0))]
           [disappeared-size
            (for/sum ([hdr disappeared])
              (or (main-mail-header-parts-message-size hdr) 0))])

      (diff-result email folder
                   (mailbox-digest-timestamp old-mbd)
                   (mailbox-digest-timestamp new-mbd)
                   (hash-count old-by-uid)
                   (hash-count new-by-uid)
                   appeared disappeared
                   appeared-size disappeared-size
                   flag-changes))))

;; ---- display ----

(define (print-diff dr verbose? full?)
  (printf "~n  ~a / ~a~n" (diff-result-email dr) (diff-result-folder dr))
  (printf "  ~a  →  ~a~n"
          (format-timestamp (diff-result-old-timestamp dr))
          (format-timestamp (diff-result-new-timestamp dr)))
  (printf "  ~a  →  ~a messages"
          (diff-result-old-count dr)
          (diff-result-new-count dr))
  (let ([delta (- (diff-result-new-count dr) (diff-result-old-count dr))])
    (printf " (~a~a)~n"
            (if (>= delta 0) "+" "")
            delta))

  (let ([app (diff-result-appeared dr)]
        [dis (diff-result-disappeared dr)]
        [fc (diff-result-flag-changes dr)])

    (when (not (null? app))
      (printf "    Appeared:    ~a messages" (length app))
      (let ([sz (diff-result-appeared-size dr)])
        (when (> sz 0) (printf " (~a)" (format-size sz))))
      (newline)
      (when (or verbose? full?)
        (let* ([sorted (sort app >
                             #:key (lambda (h)
                                     (or (main-mail-header-parts-parsed-epoch h) 0)))]
               [to-show (if full? sorted (take sorted (min 5 (length sorted))))])
          (for ([hdr to-show])
            (printf "      ~a  ~a  ~a~n"
                    (~a (main-mail-header-parts-mail-id hdr) #:min-width 7)
                    (~a (truncate-string (main-mail-header-parts-date-string hdr) 25)
                        #:min-width 27)
                    (truncate-string (main-mail-header-parts-subj hdr) 60)))
          (when (and (not full?) (> (length sorted) 5))
            (printf "      ... and ~a more~n" (- (length sorted) 5))))))

    (when (not (null? dis))
      (printf "    Disappeared: ~a messages" (length dis))
      (let ([sz (diff-result-disappeared-size dr)])
        (when (> sz 0) (printf " (~a)" (format-size sz))))
      (newline)
      (when (or verbose? full?)
        ;; Group disappeared by sender for a useful summary
        (let ([by-sender (make-hash)])
          (for ([hdr dis])
            (let ([from (string-downcase (main-mail-header-parts-from hdr))])
              (hash-update! by-sender from add1 0)))
          (let ([sorted-senders (sort (hash->list by-sender) > #:key cdr)])
            (let ([to-show (if full? sorted-senders
                               (take sorted-senders (min 10 (length sorted-senders))))])
              (for ([pair to-show])
                (printf "      ~a  ~a~n"
                        (~a (cdr pair) #:min-width 7 #:align 'right)
                        (truncate-string (car pair) 60)))
              (when (and (not full?) (> (length sorted-senders) 10))
                (printf "      ... and ~a more senders~n"
                        (- (length sorted-senders) 10))))))))

    (when (not (null? fc))
      (printf "    Flag changes: ~a messages~n" (length fc)))

    (when (and (null? app) (null? dis) (null? fc))
      (printf "    No changes.~n"))))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)]
        [account-filter #f]
        [verbose? #f]
        [full? #f])
    (let loop ([remaining arg-list])
      (cond
        [(null? remaining) (void)]
        [(and (string=? (car remaining) "--account")
              (not (null? (cdr remaining))))
         (set! account-filter (string-downcase (cadr remaining)))
         (loop (cddr remaining))]
        [(string=? (car remaining) "--verbose")
         (set! verbose? #t)
         (loop (cdr remaining))]
        [(string=? (car remaining) "--full")
         (set! full? #t)
         (set! verbose? #t)
         (loop (cdr remaining))]
        [else (loop (cdr remaining))]))
    (values account-filter verbose? full?)))

;; ---- main ----

(define (main)
  (let-values ([(account-filter verbose? full?)
                (parse-args (current-command-line-arguments))])

    (let ([grouped (load-all-digests-grouped)])

      (printf "Digest Diff Report~n")
      (printf "==================~n")

      (let ([keys (sort (hash-keys grouped) string<?
                        #:key (lambda (p) (format "~a/~a" (car p) (cdr p))))])

        ;; Filter by account if specified
        (let ([filtered-keys
               (if account-filter
                   (filter (lambda (k)
                             (string-contains? (string-downcase (car k))
                                               account-filter))
                           keys)
                   keys)])

          (for ([key filtered-keys])
            (let ([entries (hash-ref grouped key)])
              (when (>= (length entries) 2)
                ;; Compare the two most recent digests
                (let* ([older (cdr (list-ref entries (- (length entries) 2)))]
                       [newer (cdr (last entries))]
                       [dr (diff-digests older newer)])
                  (print-diff dr verbose? full?)))))

          ;; Summary
          (let ([diffable (for/sum ([key filtered-keys])
                            (let ([entries (hash-ref grouped key)])
                              (if (>= (length entries) 2) 1 0)))]
                [single (for/sum ([key filtered-keys])
                          (let ([entries (hash-ref grouped key)])
                            (if (= (length entries) 1) 1 0)))])
            (printf "~n~a account+folder pairs compared" diffable)
            (when (> single 0)
              (printf ", ~a with only one digest (no comparison possible)" single))
            (newline)))))))

(main)
