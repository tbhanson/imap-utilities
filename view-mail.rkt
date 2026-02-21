#lang racket

;; View, delete, or manage flags on IMAP messages.
;;
;; Usage:
;;   # Specific account and folder:
;;   racket view-mail.rkt <account> <folder> --uid <uid>
;;   racket view-mail.rkt <account> <folder> --flag '$Phishing'
;;   racket view-mail.rkt <account> <folder> --flag '$Phishing' --delete
;;   racket view-mail.rkt <account> <folder> --flag '$Phishing' --remove-flag
;;   racket view-mail.rkt <account> <folder> --uid 2410 --flag '$Phishing' --remove-flag
;;
;;   # Search across ALL accounts (scans local digests first):
;;   racket view-mail.rkt --flag '$Phishing'
;;   racket view-mail.rkt --flag '$Phishing' --delete
;;   racket view-mail.rkt --flag '$Phishing' --remove-flag
;;   racket view-mail.rkt --flag '$Phishing' --headers-only
;;
;; Options:
;;   --uid <uid>        Target a specific message by UID
;;   --flag <flag>      Target messages with this flag
;;   --delete           Delete matching messages
;;   --remove-flag      Remove the flag from matching messages
;;   --headers-only     Show only headers, not the body
;;
;; When --delete or --remove-flag is used with multiple messages,
;; each message is shown with an interactive prompt:
;;   y = yes (this one), n = no (skip), a = all (remaining), q = quit
;;
;; When no account/folder is given with --flag, the tool scans local
;; digests to find which accounts and folders contain messages with
;; that flag, then connects only to those.
;;
;; Deleted messages are marked with a $DeletedOnIMAPServer flag in the
;; local digest, preserving header history even after server deletion.

(require
  "src/imap-email-account-credentials.rkt"
  "src/connect-to-imap-account.rkt"
  "src/gmail-oauth2.rkt"
  "src/oauth2-details.rkt"
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  net/imap
  net/head
  openssl
  gregor
  racket/serialize)

;; ---- connection ----

(define (connect-to credential folder-name)
  (if (imap-email-account-credentials-xoauth2? credential)
      (let ([oauth2-creds (load-google-oauth2-details)]
            [email (imap-email-account-credentials-mailaddress credential)])
        (oauth2-connect-to-imap email oauth2-creds folder-name))
      (securely-connect-to-imap-account credential folder-name)))

;; ---- credential lookup ----

(define (load-credentials)
  (read-email-account-credentials-hash-from-file-named
   (default-credentials-filepath)))

(define (email->credential creds email)
  (for/first ([name (hash-keys creds)]
              #:when (string=? (imap-email-account-credentials-mailaddress
                                (hash-ref creds name))
                               email))
    (hash-ref creds name)))

(define (email->account-name creds email)
  (for/first ([name (hash-keys creds)]
              #:when (string=? (imap-email-account-credentials-mailaddress
                                (hash-ref creds name))
                               email))
    name))

;; ---- UID to sequence number mapping ----

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
                [(null? remaining) (loop (+ end 1))]
                [(= (first (car remaining)) target-uid) seqno]
                [else (check (cdr remaining) (+ seqno 1))])))))))

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

;; ---- digest scanning (for cross-account flag search) ----

;; Returns list of (email folder-name uid-list) for digests containing the flag.
(define (scan-digests-for-flag target-flag)
  (let ([dir (default-digest-dir)]
        [flag-sym (string->symbol target-flag)]
        [by-key (make-hash)])
    (when (directory-exists? dir)
      ;; Load latest digest per account+folder
      (let ([latest (make-hash)])
        (for ([f (directory-list dir #:build? #t)]
              #:when (regexp-match? #rx"\\.ser$" (path->string f)))
          (with-handlers ([exn:fail? (lambda (e) (void))])
            (let* ([mbd (load-mailbox-digest-from-file f)]
                   [key (cons (mailbox-digest-mail-address mbd)
                              (mailbox-digest-folder-name mbd))])
              (let ([existing (hash-ref latest key #f)])
                (when (or (not existing)
                          (datetime>? (mailbox-digest-timestamp mbd)
                                      (mailbox-digest-timestamp existing)))
                  (hash-set! latest key mbd))))))
        ;; Find which have the flag
        (for ([(key mbd) (in-hash latest)])
          (let ([matching-uids
                 (for/list ([hdr (mailbox-digest-mail-headers mbd)]
                            #:when (member flag-sym (main-mail-header-parts-flags hdr)))
                   (main-mail-header-parts-mail-id hdr))])
            (when (not (null? matching-uids))
              (hash-set! by-key key matching-uids))))))
    (for/list ([(key uids) (in-hash by-key)])
      (list (car key) (cdr key) uids))))

;; ---- digest tombstoning ----

;; Mark UIDs in a digest with $LocalDeleted and re-save.
(define (mark-deleted-in-digest email folder-name deleted-uids)
  (let ([digest-path (find-latest-digest-for email folder-name)])
    (when digest-path
      (let* ([mbd (load-mailbox-digest-from-file digest-path)]
             [uid-set (list->set deleted-uids)]
             [updated-headers
              (for/list ([hdr (mailbox-digest-mail-headers mbd)])
                (if (set-member? uid-set (main-mail-header-parts-mail-id hdr))
                    ;; Add $LocalDeleted to flags
                    (let ([new-flags (if (member '|$LocalDeleted|
                                                 (main-mail-header-parts-flags hdr))
                                        (main-mail-header-parts-flags hdr)
                                        (cons '|$LocalDeleted|
                                              (main-mail-header-parts-flags hdr)))])
                      (main-mail-header-parts
                       (main-mail-header-parts-mail-id hdr)
                       (main-mail-header-parts-date-string hdr)
                       (main-mail-header-parts-from hdr)
                       (main-mail-header-parts-to hdr)
                       (main-mail-header-parts-cc hdr)
                       (main-mail-header-parts-bcc hdr)
                       (main-mail-header-parts-subj hdr)
                       new-flags))
                    hdr))]
             [updated-digest
              (mailbox-digest
               (mailbox-digest-mail-address mbd)
               (mailbox-digest-folder-name mbd)
               (mailbox-digest-uid-validity mbd)
               (mailbox-digest-index-range mbd)
               updated-headers
               (mailbox-digest-timestamp mbd))])
        ;; Overwrite the existing digest file
        (call-with-output-file digest-path
          (lambda (out) (write (serialize updated-digest) out))
          #:exists 'replace)
        (printf "  Marked ~a message(s) as $LocalDeleted in local digest.~n"
                (length deleted-uids))))))

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

      (for ([field-name (list #"date" #"from" #"to" #"cc" #"subject" #"content-type")])
        (with-handlers ([exn:fail? (lambda (e) (void))])
          (let ([val (bytes->string/utf-8 (extract-field field-name header))])
            (printf "  ~a: ~a~n"
                    (string-titlecase (bytes->string/utf-8 field-name))
                    val))))

      (when (and body (not headers-only?))
        (printf "~n--- Body ---~n")
        (let ([body-str (with-handlers ([exn:fail?
                                         (lambda (e)
                                           (bytes->string/latin-1 body))])
                          (bytes->string/utf-8 body))])
          (let ([lines (string-split body-str "\r\n")])
            (for ([line (take lines (min (length lines) 200))])
              (printf "~a~n" line))
            (when (> (length lines) 200)
              (printf "~n... (~a more lines truncated)~n" (- (length lines) 200))))))

      (printf "~n================================================================~n"))))

;; ---- server operations ----

(define (delete-messages! imap-conn seqnos)
  (imap-store imap-conn '+ (sort seqnos >) (list (symbol->imap-flag 'deleted)))
  (imap-expunge imap-conn)
  (printf "  Deleted and expunged ~a message(s).~n" (length seqnos)))

(define (remove-flag-from-messages! imap-conn seqnos flag-sym)
  (imap-store imap-conn '- seqnos (list flag-sym))
  (printf "  Removed flag ~a from ~a message(s).~n" flag-sym (length seqnos)))

;; ---- interactive prompt ----

;; Ask user about an action. Returns 'yes, 'no, 'all, or 'quit.
(define (ask-ynaq prompt)
  (printf "~a [y/N/a(ll)/q(uit)] " prompt)
  (flush-output)
  (let ([answer (read-line)])
    (cond
      [(not answer) 'no]
      [(regexp-match? #rx"^[yY]$" (string-trim answer)) 'yes]
      [(regexp-match? #rx"^[aA]" (string-trim answer)) 'all]
      [(regexp-match? #rx"^[qQ]" (string-trim answer)) 'quit]
      [else 'no])))

;; Ask a simple y/n question. Returns #t or #f.
(define (ask-yn prompt)
  (printf "~a [y/N] " prompt)
  (flush-output)
  (let ([answer (read-line)])
    (and answer (regexp-match? #rx"^[yY]" answer))))

;; ---- process one account+folder ----

(define (process-folder credential email folder-name target-flag
                        delete? remove-flag? headers-only?
                        #:filter-uids [filter-uids #f])
  (let ([account-name (imap-email-account-credentials-accountname credential)])
    (printf "~n========================================~n")
    (printf "  ~a (~a) / ~a~n" account-name email folder-name)
    (printf "========================================~n")
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (printf "ERROR: ~a~n" (exn-message e)))])
      (let* ([imap-conn (connect-to credential folder-name)]
             [msg-count (imap-messages imap-conn)]
             [flag-sym (string->symbol target-flag)])

        (printf "Searching ~a messages for flag ~a...~n" msg-count target-flag)
        (let* ([all-matches (find-seqnos-for-flag imap-conn flag-sym msg-count)]
               [matches (if filter-uids
                            (filter (lambda (m) (member (cdr m) filter-uids))
                                    all-matches)
                            all-matches)])
          (if (null? matches)
              (printf "No messages found with flag ~a.~n" target-flag)
              (begin
                (printf "Found ~a message(s).~n~n" (length matches))

                (cond
                  ;; Batch delete: show all, then confirm once
                  [(and delete? (not remove-flag?))
                   (for ([match matches])
                     (display-message imap-conn (car match) (cdr match) headers-only?))
                   (when (ask-yn (format "~nDelete all ~a message(s)?" (length matches)))
                     (delete-messages! imap-conn (map car matches))
                     (mark-deleted-in-digest email folder-name (map cdr matches)))]

                  ;; Batch remove-flag: show all, then confirm once
                  [(and remove-flag? (not delete?))
                   (for ([match matches])
                     (display-message imap-conn (car match) (cdr match) headers-only?))
                   (when (ask-yn (format "~nRemove flag ~a from all ~a message(s)?"
                                         target-flag (length matches)))
                     (remove-flag-from-messages! imap-conn (map car matches) flag-sym))]

                  ;; Interactive: show each, ask per message
                  [(and (or delete? remove-flag?) (> (length matches) 1))
                   ;; Shouldn't normally hit this branch with current flags,
                   ;; but here for future use
                   (void)]

                  ;; View only (no action flags)
                  [else
                   (for ([match matches])
                     (display-message imap-conn (car match) (cdr match)
                                      headers-only?))]))))

        (imap-disconnect imap-conn)))))

;; Like process-folder but with interactive per-message prompts.
;; For each message, displays it then asks y/n/all/quit.
(define (process-folder-interactive credential email folder-name target-flag
                                    action headers-only?
                                    #:auto-confirm? [auto-confirm? #f])
  (let ([account-name (imap-email-account-credentials-accountname credential)])
    (printf "~n========================================~n")
    (printf "  ~a (~a) / ~a~n" account-name email folder-name)
    (printf "========================================~n")
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (printf "ERROR: ~a~n" (exn-message e)))])
      (let* ([imap-conn (connect-to credential folder-name)]
             [msg-count (imap-messages imap-conn)]
             [flag-sym (string->symbol target-flag)]
             [action-label (if (eq? action 'delete) "Delete" "Remove flag from")])

        (printf "Searching ~a messages for flag ~a...~n" msg-count target-flag)
        (let ([matches (find-seqnos-for-flag imap-conn flag-sym msg-count)])
          (if (null? matches)
              (printf "No messages found with flag ~a.~n" target-flag)
              (begin
                (printf "Found ~a message(s).~n" (length matches))
                (let ([acted-seqnos '()]
                      [acted-uids '()]
                      [do-all? auto-confirm?]
                      [stopped? #f])
                  (for ([match matches]
                        #:break stopped?)
                    (let ([seqno (car match)]
                          [uid (cdr match)])
                      (display-message imap-conn seqno uid headers-only?)
                      (let ([decision (if do-all? 'yes
                                         (ask-ynaq (format "~a this message?" action-label)))])
                        (case decision
                          [(yes)
                           (set! acted-seqnos (cons seqno acted-seqnos))
                           (set! acted-uids (cons uid acted-uids))]
                          [(all)
                           (set! do-all? #t)
                           (set! acted-seqnos (cons seqno acted-seqnos))
                           (set! acted-uids (cons uid acted-uids))]
                          [(quit)
                           (set! stopped? #t)]
                          [else (void)]))))

                  (when (not (null? acted-seqnos))
                    (case action
                      [(delete)
                       (delete-messages! imap-conn (sort acted-seqnos >))
                       (mark-deleted-in-digest email folder-name acted-uids)]
                      [(remove-flag)
                       (remove-flag-from-messages! imap-conn acted-seqnos flag-sym)]))

                  (printf "~n  ~a of ~a message(s) ~a.~n"
                          (length acted-seqnos)
                          (length matches)
                          (if (eq? action 'delete) "deleted" "had flag removed"))))))

        (imap-disconnect imap-conn)))))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)]
        [account-name #f]
        [folder-name #f]
        [target-uid #f]
        [target-flag #f]
        [delete? #f]
        [remove-flag? #f]
        [headers-only? #f]
        [auto-confirm? #f]
        [positional '()])
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
        [(string=? (car remaining) "--remove-flag")
         (set! remove-flag? #t)
         (loop (cdr remaining))]
        [(string=? (car remaining) "--headers-only")
         (set! headers-only? #t)
         (loop (cdr remaining))]
        [(or (string=? (car remaining) "--yes")
             (string=? (car remaining) "-y"))
         (set! auto-confirm? #t)
         (loop (cdr remaining))]
        [(string-prefix? (car remaining) "--")
         (loop (cdr remaining))]
        [else
         (set! positional (append positional (list (car remaining))))
         (loop (cdr remaining))]))
    (when (>= (length positional) 1)
      (set! account-name (first positional)))
    (when (>= (length positional) 2)
      (set! folder-name (second positional)))
    (values account-name folder-name target-uid target-flag
            delete? remove-flag? headers-only? auto-confirm?)))

;; ---- main ----

(define (main)
  (let-values ([(account-name folder-name target-uid target-flag
                 delete? remove-flag? headers-only? auto-confirm?)
                (parse-args (current-command-line-arguments))])

    (let ([creds (load-credentials)])

      (cond
        ;; Mode 1: specific account + folder + UID (with optional flag for --remove-flag)
        [(and account-name folder-name target-uid)
         (unless (hash-has-key? creds account-name)
           (printf "No account named ~s.~n" account-name)
           (exit 1))
         (let* ([credential (hash-ref creds account-name)]
                [email (imap-email-account-credentials-mailaddress credential)]
                [imap-conn (connect-to credential folder-name)]
                [msg-count (imap-messages imap-conn)])
           (printf "Searching for UID ~a in ~a messages...~n" target-uid msg-count)
           (let ([seqno (find-seqno-for-uid imap-conn target-uid msg-count)])
             (if seqno
                 (begin
                   (display-message imap-conn seqno target-uid headers-only?)
                   (cond
                     [delete?
                      (when (or auto-confirm? (ask-yn "Delete this message?"))
                        (delete-messages! imap-conn (list seqno))
                        (mark-deleted-in-digest email folder-name
                                                (list target-uid)))]
                     [(and remove-flag? target-flag)
                      (when (or auto-confirm?
                                (ask-yn (format "Remove flag ~a?" target-flag)))
                        (remove-flag-from-messages!
                         imap-conn (list seqno)
                         (string->symbol target-flag)))]))
                 (printf "UID ~a not found in this folder.~n" target-uid)))
           (imap-disconnect imap-conn))]

        ;; Mode 2: specific account + folder + flag
        [(and account-name folder-name target-flag)
         (unless (hash-has-key? creds account-name)
           (printf "No account named ~s.~n" account-name)
           (exit 1))
         (let* ([credential (hash-ref creds account-name)]
                [email (imap-email-account-credentials-mailaddress credential)])
           (if (or delete? remove-flag?)
               (process-folder-interactive
                credential email folder-name target-flag
                (if delete? 'delete 'remove-flag) headers-only?
                #:auto-confirm? auto-confirm?)
               (process-folder credential email folder-name target-flag
                               #f #f headers-only?)))]

        ;; Mode 3: cross-account flag search (no account/folder given)
        [(and target-flag (not account-name))
         (printf "Scanning local digests for flag ~a...~n" target-flag)
         (let ([hits (scan-digests-for-flag target-flag)])
           (if (null? hits)
               (printf "No messages with flag ~a found in any local digest.~n"
                       target-flag)
               (begin
                 (printf "Found flag ~a in:~n" target-flag)
                 (let ([total-msgs 0])
                   (for ([hit hits])
                     (let ([email (first hit)]
                           [folder (second hit)]
                           [uids (third hit)])
                       (set! total-msgs (+ total-msgs (length uids)))
                       (printf "  ~a / ~a: ~a message(s)~n"
                               email folder (length uids))))
                   (printf "  Total: ~a message(s)~n" total-msgs))
                 (printf "~nConnecting to each...~n")

                 (for ([hit hits])
                   (let ([email (first hit)]
                         [folder (second hit)]
                         [uids (third hit)])
                     (let ([credential (email->credential creds email)])
                       (if credential
                           (cond
                             ;; Filter to specific UID if given
                             [(and target-uid (not (member target-uid uids)))
                              (void)]  ; UID not in this folder, skip
                             [target-uid
                              ;; Connect and process just this UID
                              (process-folder credential email folder target-flag
                                              delete? remove-flag? headers-only?
                                              #:filter-uids (list target-uid))]
                             ;; Interactive mode for delete/remove-flag
                             [(or delete? remove-flag?)
                              (process-folder-interactive
                               credential email folder target-flag
                               (if delete? 'delete 'remove-flag) headers-only?
                               #:auto-confirm? auto-confirm?)]
                             ;; View only
                             [else
                              (process-folder credential email folder target-flag
                                              #f #f headers-only?)])
                           (printf "~nWARNING: no credential found for ~a~n"
                                   email))))))))]

        ;; Usage
        [else
         (printf "Usage:~n")
         (printf "  racket view-mail.rkt <account> <folder> --uid <uid>~n")
         (printf "  racket view-mail.rkt <account> <folder> --flag '<flag>'~n")
         (printf "  racket view-mail.rkt <account> <folder> --uid <uid> --flag '<flag>' --remove-flag~n")
         (printf "  racket view-mail.rkt --flag '<flag>'   (search all accounts)~n")
         (printf "~nOptions:~n")
         (printf "  --delete           Delete matching messages~n")
         (printf "  --remove-flag      Remove the flag from matching messages~n")
         (printf "  --headers-only     Show only headers, not the body~n")
         (printf "  --yes / -y         Skip confirmation prompts~n")
         (exit 1)]))

    (printf "~nDone.~n")))

(main)
