#lang racket

;; Inspect saved digests and report on field population.
;;
;; Usage:
;;   racket inspect-digest.rkt latest          ; inspect most recent digest
;;   racket inspect-digest.rkt each            ; detailed report for every digest
;;   racket inspect-digest.rkt all             ; combined summary across all digests
;;   racket inspect-digest.rkt <path-to-.ser>  ; inspect a specific file
;;
;; Useful for sanity-checking data quality after fetching.

(require
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  gregor)

;; ---- per-digest inspection ----

(define (inspect-one-digest mbd)
  (let ([total (mailbox-digest-count mbd)]
        [non-empty-date 0]
        [non-empty-from 0]
        [non-empty-to 0]
        [non-empty-cc 0]
        [non-empty-bcc 0]
        [non-empty-subj 0]
        [has-flags 0]
        [flag-counts (make-hash)]
        [sample-subjects '()])

    (for ([hdr (mailbox-digest-mail-headers mbd)])
      (unless (string=? (main-mail-header-parts-date-string hdr) "")
        (set! non-empty-date (add1 non-empty-date)))
      (unless (string=? (main-mail-header-parts-from hdr) "")
        (set! non-empty-from (add1 non-empty-from)))
      (unless (string=? (main-mail-header-parts-to hdr) "")
        (set! non-empty-to (add1 non-empty-to)))
      (unless (string=? (main-mail-header-parts-cc hdr) "")
        (set! non-empty-cc (add1 non-empty-cc)))
      (unless (string=? (main-mail-header-parts-bcc hdr) "")
        (set! non-empty-bcc (add1 non-empty-bcc)))
      (let ([subj (main-mail-header-parts-subj hdr)])
        (unless (string=? subj "")
          (set! non-empty-subj (add1 non-empty-subj))
          (when (< (length sample-subjects) 5)
            (set! sample-subjects (cons subj sample-subjects)))))
      (let ([flags (main-mail-header-parts-flags hdr)])
        (when (not (null? flags))
          (set! has-flags (add1 has-flags)))
        (for ([flag flags])
          (hash-update! flag-counts flag add1 0))))

    (printf "~n  ~a / ~a  (~a messages, fetched ~a)~n"
            (mailbox-digest-mail-address mbd)
            (mailbox-digest-folder-name mbd)
            total
            (~t (mailbox-digest-timestamp mbd) "yyyy-MM-dd HH:mm:ss"))
    (printf "  UID validity: ~a~n" (mailbox-digest-uid-validity mbd))
    (printf "  Index range: ~a to ~a~n"
            (car (mailbox-digest-index-range mbd))
            (cdr (mailbox-digest-index-range mbd)))
    (printf "~n  Field population:~n")

    (define (pct n) (if (= total 0) 0 (round (* 100.0 (/ n total)))))

    (printf "    date:    ~a / ~a (~a%)~n" non-empty-date total (pct non-empty-date))
    (printf "    from:    ~a / ~a (~a%)~n" non-empty-from total (pct non-empty-from))
    (printf "    to:      ~a / ~a (~a%)~n" non-empty-to total (pct non-empty-to))
    (printf "    cc:      ~a / ~a (~a%)~n" non-empty-cc total (pct non-empty-cc))
    (printf "    bcc:     ~a / ~a (~a%)~n" non-empty-bcc total (pct non-empty-bcc))
    (printf "    subject: ~a / ~a (~a%)~n" non-empty-subj total (pct non-empty-subj))
    (printf "    flags:   ~a / ~a (~a%)~n" has-flags total (pct has-flags))

    (when (not (hash-empty? flag-counts))
      (printf "~n  Flags found:~n")
      (for ([pair (sort (hash->list flag-counts) > #:key cdr)])
        (printf "    ~a: ~a~n" (car pair) (cdr pair))))

    (when (not (null? sample-subjects))
      (printf "~n  Sample subjects:~n")
      (for ([s sample-subjects])
        (printf "    ~s~n" (if (> (string-length s) 80)
                               (string-append (substring s 0 77) "...")
                               s))))))

;; ---- combined summary ----

(define (inspect-all-combined digests)
  (let ([total-messages 0]
        [total-digests (length digests)]
        [total-date 0]
        [total-from 0]
        [total-to 0]
        [total-cc 0]
        [total-bcc 0]
        [total-subj 0]
        [total-flags 0]
        [flag-counts (make-hash)])

    (for ([mbd digests])
      (let ([count (mailbox-digest-count mbd)])
        (set! total-messages (+ total-messages count))
        (for ([hdr (mailbox-digest-mail-headers mbd)])
          (unless (string=? (main-mail-header-parts-date-string hdr) "")
            (set! total-date (add1 total-date)))
          (unless (string=? (main-mail-header-parts-from hdr) "")
            (set! total-from (add1 total-from)))
          (unless (string=? (main-mail-header-parts-to hdr) "")
            (set! total-to (add1 total-to)))
          (unless (string=? (main-mail-header-parts-cc hdr) "")
            (set! total-cc (add1 total-cc)))
          (unless (string=? (main-mail-header-parts-bcc hdr) "")
            (set! total-bcc (add1 total-bcc)))
          (unless (string=? (main-mail-header-parts-subj hdr) "")
            (set! total-subj (add1 total-subj)))
          (let ([flags (main-mail-header-parts-flags hdr)])
            (when (not (null? flags))
              (set! total-flags (add1 total-flags)))
            (for ([flag flags])
              (hash-update! flag-counts flag add1 0))))))

    (define (pct n) (if (= total-messages 0) 0 (round (* 100.0 (/ n total-messages)))))

    (printf "~n  Combined summary: ~a digests, ~a messages~n~n" total-digests total-messages)

    (printf "  Digests:~n")
    (for ([mbd (sort digests string<?
                     #:key (lambda (d) (format "~a/~a"
                                               (mailbox-digest-mail-address d)
                                               (mailbox-digest-folder-name d))))])
      (printf "    ~a / ~a: ~a messages~n"
              (mailbox-digest-mail-address mbd)
              (mailbox-digest-folder-name mbd)
              (mailbox-digest-count mbd)))

    (printf "~n  Field population (across all digests):~n")
    (printf "    date:    ~a / ~a (~a%)~n" total-date total-messages (pct total-date))
    (printf "    from:    ~a / ~a (~a%)~n" total-from total-messages (pct total-from))
    (printf "    to:      ~a / ~a (~a%)~n" total-to total-messages (pct total-to))
    (printf "    cc:      ~a / ~a (~a%)~n" total-cc total-messages (pct total-cc))
    (printf "    bcc:     ~a / ~a (~a%)~n" total-bcc total-messages (pct total-bcc))
    (printf "    subject: ~a / ~a (~a%)~n" total-subj total-messages (pct total-subj))
    (printf "    flags:   ~a / ~a (~a%)~n" total-flags total-messages (pct total-flags))

    (when (not (hash-empty? flag-counts))
      (printf "~n  Flags found (all digests combined):~n")
      (for ([pair (sort (hash->list flag-counts) > #:key cdr)])
        (printf "    ~a: ~a~n" (car pair) (cdr pair))))))

;; ---- loading helpers ----

(define (find-latest-digest-file)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (let ([files (sort
                      (for/list ([f (directory-list dir #:build? #t)]
                                 #:when (regexp-match? #rx"\\.ser$" (path->string f)))
                        f)
                      string>?
                      #:key path->string)])
          (if (null? files)
              (begin (printf "No saved digests found.~n") #f)
              (first files)))
        (begin (printf "Digest directory does not exist.~n") #f))))

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

(define (load-all-digests)
  (let ([files (all-digest-files)])
    (for/fold ([result '()])
              ([f files])
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (printf "Warning: could not read ~a: ~a~n"
                                 (file-name-from-path f) (exn-message e))
                         (values result))])
        (values (cons (load-mailbox-digest-from-file f) result))))))

;; ---- main ----

(define (main)
  (let ([args (current-command-line-arguments)])
    (when (< (vector-length args) 1)
      (printf "Usage: racket inspect-digest.rkt <latest | each | all | path-to-.ser>~n")
      (printf "  latest  — inspect the most recently saved digest~n")
      (printf "  each    — detailed report for every saved digest~n")
      (printf "  all     — combined summary across all digests~n")
      (exit 1))

    (let ([arg (vector-ref args 0)])
      (printf "============================================================~n")
      (printf "  Digest Inspection Report~n")
      (printf "============================================================~n")
      (cond
        [(string=? arg "latest")
         (let ([path (find-latest-digest-file)])
           (when path
             (printf "~nLoading ~a ...~n" (file-name-from-path path))
             (inspect-one-digest (load-mailbox-digest-from-file path))))]

        [(string=? arg "each")
         (let ([files (all-digest-files)])
           (printf "~nFound ~a digest files.~n" (length files))
           (for ([f files])
             (printf "~nLoading ~a ...~n" (file-name-from-path f))
             (with-handlers ([exn:fail?
                              (lambda (e)
                                (printf "  ERROR: ~a~n" (exn-message e)))])
               (inspect-one-digest (load-mailbox-digest-from-file f)))))]

        [(string=? arg "all")
         (printf "~nLoading all digests...~n")
         (let ([digests (load-all-digests)])
           (if (null? digests)
               (printf "No digests found.~n")
               (inspect-all-combined digests)))]

        [else
         (let ([path (string->path arg)])
           (unless (file-exists? path)
             (printf "File not found: ~a~n" arg)
             (exit 1))
           (inspect-one-digest (load-mailbox-digest-from-file path)))])

      (printf "~n============================================================~n"))))

(main)
