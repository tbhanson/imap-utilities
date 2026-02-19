#lang racket

;; View or delete messages on an IMAP server.
;;
;; Usage:
;;   racket view-mail.rkt <account> <folder> --uid <uid>
;;   racket view-mail.rkt <account> <folder> --flag <flag>
;;   racket view-mail.rkt <account> <folder> --flag <flag> --delete
;;
;; Examples:
;;   racket view-mail.rkt "gmail-x" "INBOX" --uid 54321
;;   racket view-mail.rkt "pwd-mail-y" "INBOX" --flag '$Phishing'
;;   racket view-mail.rkt "pwd-mail-y" "INBOX" --flag '$Phishing' --delete
;;
;; --uid <uid>       View the message with this UID
;; --flag <flag>     View all messages with this flag
;; --delete          After viewing, delete the matching messages
;;                   (asks for confirmation first)
;; --headers-only    Show only headers, not the body
;;
;; The tool connects live to the IMAP server. UIDs come from our
;; digest data (inspect with report.rkt or find-unread.rkt).

(require
  "src/imap-email-account-credentials.rkt"
  "src/connect-to-imap-account.rkt"
  "src/gmail-oauth2.rkt"
  "src/oauth2-details.rkt"
  net/imap
  net/head
  openssl)

;; ---- connection ----

(define (connect-to credential folder-name)
  (if (imap-email-account-credentials-xoauth2? credential)
      (let ([oauth2-creds (load-google-oauth2-details)]
            [email (imap-email-account-credentials-mailaddress credential)])
        (oauth2-connect-to-imap email oauth2-creds folder-name))
      (securely-connect-to-imap-account credential folder-name)))

;; ---- UID to sequence number mapping ----

;; Fetch UIDs for all messages and build a UID->seqno hash.
;; For large mailboxes this is slow, so we do it in batches
;; and stop early if we find what we need.
(define (find-seqno-for-uid imap-conn target-uid msg-count)
  (let ([batch-size 500])
    (let loop ([start 1])
      (if (> start msg-count)
          #f
          (let* ([end (min msg-count (+ start batch-size -1))]
                 [indices (for/list ([i (in-range start (+ end 1))]) i)]
                 [results (imap-get-messages imap-conn indices '(uid))])
            (let check ([remaining results]
                        [seqno start])
              (cond
                [(null? remaining)
                 (loop (+ end 1))]
                [(= (first (car remaining)) target-uid)
                 seqno]
                [else
                 (check (cdr remaining) (+ seqno 1))])))))))

;; Find all sequence numbers for messages with a given flag.
(define (find-seqnos-for-flag imap-conn flag-sym msg-count)
  (let ([batch-size 500]
        [matches '()])
    (let loop ([start 1])
      (if (> start msg-count)
          (reverse matches)
          (let* ([end (min msg-count (+ start batch-size -1))]
                 [indices (for/list ([i (in-range start (+ end 1))]) i)]
                 [results (imap-get-messages imap-conn indices '(uid flags))])
            (for ([result results]
                  [seqno (in-range start (+ end 1))])
              (let ([uid (first result)]
                    [flags (second result)])
                (when (member flag-sym flags)
                  (set! matches (cons (cons seqno uid) matches)))))
            (loop (+ end 1)))))))

;; ---- display ----

(define (display-message imap-conn seqno uid headers-only?)
  (let* ([fields (if headers-only? '(uid header flags) '(uid header body flags))]
         [results (imap-get-messages imap-conn (list seqno) fields)]
         [result (first results)])
    (let ([msg-uid (first result)]
          [header (second result)]
          [body (if headers-only? #f (third result))]
          [flags (if headers-only? (third result) (fourth result))])

      (printf "~n================================================================~n")
      (printf "  UID: ~a  (sequence #~a)~n" msg-uid seqno)
      (printf "  Flags: ~a~n" flags)
      (printf "================================================================~n")

      ;; Display key headers
      (for ([field-name (list #"date" #"from" #"to" #"cc" #"subject" #"content-type")])
        (with-handlers ([exn:fail? (lambda (e) (void))])
          (let ([val (bytes->string/utf-8 (extract-field field-name header))])
            (printf "  ~a: ~a~n"
                    (string-titlecase (bytes->string/utf-8 field-name))
                    val))))

      (when (and body (not headers-only?))
        (printf "~n--- Body ---~n")
        ;; Body is bytes with CRLF line endings
        (let ([body-str (with-handlers ([exn:fail?
                                         (lambda (e)
                                           ;; Fall back to latin-1 if UTF-8 fails
                                           (bytes->string/latin-1 body))])
                          (bytes->string/utf-8 body))])
          ;; Show first ~200 lines to avoid flooding terminal
          (let ([lines (string-split body-str "\r\n")])
            (for ([line (take lines (min (length lines) 200))])
              (printf "~a~n" line))
            (when (> (length lines) 200)
              (printf "~n... (~a more lines truncated)~n" (- (length lines) 200))))))

      (printf "~n================================================================~n"))))

;; ---- deletion ----

(define (delete-messages! imap-conn seqnos)
  ;; IMAP deletion: flag as \Deleted then EXPUNGE
  ;; Must process in reverse order since expunge changes sequence numbers
  (imap-store imap-conn '+ (sort seqnos >) (list (symbol->imap-flag 'deleted)))
  (imap-expunge imap-conn)
  (printf "Deleted and expunged ~a message(s).~n" (length seqnos)))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)]
        [account-name #f]
        [folder-name #f]
        [target-uid #f]
        [target-flag #f]
        [delete? #f]
        [headers-only? #f])
    ;; First two positional args are account and folder
    (let ([positional (filter (lambda (a) (not (string-prefix? a "--"))) arg-list)])
      (when (>= (length positional) 1)
        (set! account-name (first positional)))
      (when (>= (length positional) 2)
        (set! folder-name (second positional))))
    ;; Named args
    (let loop ([remaining arg-list])
      (cond
        [(null? remaining) (void)]
        [(and (string=? (car remaining) "--uid")
              (not (null? (cdr remaining))))
         (set! target-uid (string->number (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--flag")
              (not (null? (cdr remaining))))
         (set! target-flag (cadr remaining))
         (loop (cddr remaining))]
        [(string=? (car remaining) "--delete")
         (set! delete? #t)
         (loop (cdr remaining))]
        [(string=? (car remaining) "--headers-only")
         (set! headers-only? #t)
         (loop (cdr remaining))]
        [else (loop (cdr remaining))]))
    (values account-name folder-name target-uid target-flag delete? headers-only?)))

;; ---- main ----

(define (main)
  (let-values ([(account-name folder-name target-uid target-flag delete? headers-only?)
                (parse-args (current-command-line-arguments))])

    (unless (and account-name folder-name (or target-uid target-flag))
      (printf "Usage: racket view-mail.rkt <account> <folder> --uid <uid>~n")
      (printf "       racket view-mail.rkt <account> <folder> --flag <flag>~n")
      (printf "       racket view-mail.rkt <account> <folder> --flag <flag> --delete~n")
      (printf "~nOptions:~n")
      (printf "  --uid <uid>        View message with this UID~n")
      (printf "  --flag <flag>      View all messages with this flag~n")
      (printf "  --delete           Delete matching messages (with confirmation)~n")
      (printf "  --headers-only     Show only headers, not the body~n")
      (exit 1))

    (let ([creds (read-email-account-credentials-hash-from-file-named
                  (default-credentials-filepath))])
      (unless (hash-has-key? creds account-name)
        (printf "No account named ~s. Available:~n" account-name)
        (for ([name (sort (hash-keys creds) string<?)])
          (printf "  ~a~n" name))
        (exit 1))

      (let* ([credential (hash-ref creds account-name)]
             [imap-conn (connect-to credential folder-name)]
             [msg-count (imap-messages imap-conn)])

        (cond
          ;; View by UID
          [target-uid
           (printf "Searching for UID ~a in ~a messages...~n" target-uid msg-count)
           (let ([seqno (find-seqno-for-uid imap-conn target-uid msg-count)])
             (if seqno
                 (begin
                   (display-message imap-conn seqno target-uid headers-only?)
                   (when delete?
                     (printf "~nDelete this message? [y/N] ")
                     (flush-output)
                     (let ([answer (read-line)])
                       (if (and answer (regexp-match? #rx"^[yY]" answer))
                           (begin
                             (delete-messages! imap-conn (list seqno))
                             (printf "Message deleted.~n"))
                           (printf "Not deleted.~n")))))
                 (printf "UID ~a not found in this folder.~n" target-uid)))]

          ;; View/delete by flag
          [target-flag
           (let ([flag-sym (string->symbol target-flag)])
             (printf "Searching for messages with flag ~a in ~a messages...~n"
                     target-flag msg-count)
             (let ([matches (find-seqnos-for-flag imap-conn flag-sym msg-count)])
               (if (null? matches)
                   (printf "No messages found with flag ~a.~n" target-flag)
                   (begin
                     (printf "Found ~a message(s) with flag ~a.~n~n"
                             (length matches) target-flag)
                     (for ([match matches])
                       (let ([seqno (car match)]
                             [uid (cdr match)])
                         (display-message imap-conn seqno uid headers-only?)))

                     (when delete?
                       (printf "~nDelete all ~a message(s) with flag ~a? [y/N] "
                               (length matches) target-flag)
                       (flush-output)
                       (let ([answer (read-line)])
                         (if (and answer (regexp-match? #rx"^[yY]" answer))
                             (delete-messages! imap-conn (map car matches))
                             (printf "Not deleted.~n"))))))))])

        (imap-disconnect imap-conn)
        (printf "~nDone.~n")))))

(main)
