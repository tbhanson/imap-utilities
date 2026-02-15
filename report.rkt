#lang racket

;; Load a saved mailbox digest and print a statistics report.
;;
;; Usage:
;;   racket report.rkt <digest-file>
;;   racket report.rkt latest
;;   racket report.rkt list
;;
;; Examples:
;;   racket report.rkt ~/.imap_secrets/digests/mailbox-digest_2025-07-01_10:30:00_me@example.com_INBOX.ser
;;   racket report.rkt latest           ; report on the most recently saved digest
;;   racket report.rkt list             ; list all saved digests
;;
;; If ~/.imap_secrets/known-contacts exists, senders will be classified
;; as known or unknown. Create this file with one email address per line.

(require
  "src/mailbox-digest.rkt"
  "src/digest-report.rkt"
  "src/known-contacts.rkt")

(define (list-saved-digests)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (let ([files (sort
                      (for/list ([f (directory-list dir #:build? #t)]
                                 #:when (regexp-match? #rx"\\.ser$" (path->string f)))
                        f)
                      string<?
                      #:key path->string)])
          (if (null? files)
              (printf "No saved digests in ~a~n" dir)
              (begin
                (printf "Saved digests in ~a:~n~n" dir)
                (for ([f files]
                      [i (in-naturals 1)])
                  (printf "  ~a. ~a~n" i (file-name-from-path f))))))
        (printf "Digest directory does not exist: ~a~n" dir))))

(define (find-latest-digest)
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
        (begin (printf "Digest directory does not exist: ~a~n" dir) #f))))

(define (main)
  (let ([args (current-command-line-arguments)])
    (when (< (vector-length args) 1)
      (printf "Usage: racket report.rkt <digest-file | latest | list>~n")
      (exit 1))

    (let ([arg (vector-ref args 0)])
      (cond
        [(string=? arg "list")
         (list-saved-digests)]

        [(string=? arg "latest")
         (let ([path (find-latest-digest)])
           (when path
             (printf "Loading ~a ...~n" (file-name-from-path path))
             (run-report path)))]

        [else
         (let ([path (string->path arg)])
           (unless (file-exists? path)
             (printf "File not found: ~a~n" arg)
             (exit 1))
           (run-report path))]))))

(define (run-report digest-path)
  (let ([mbd (time (load-mailbox-digest-from-file digest-path))]
        [known-set (load-known-contacts (default-known-contacts-filepath))])
    (digest-report mbd known-set)))

(main)
