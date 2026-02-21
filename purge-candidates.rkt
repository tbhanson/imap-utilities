#lang racket

;; Find purge candidates: bulk senders not in your known-contacts.
;;
;; Usage:
;;   # List senders not in known-contacts, sorted by message count:
;;   racket purge-candidates.rkt
;;   racket purge-candidates.rkt --min 50          ; only senders with 50+ messages
;;   racket purge-candidates.rkt --before 2023-01-01  ; only count old messages
;;   racket purge-candidates.rkt --year 2020       ; only count messages from 2020
;;
;;   # Show what would be deleted for a specific sender:
;;   racket purge-candidates.rkt --from noreply@github.com
;;   racket purge-candidates.rkt --from noreply@github.com --before 2024-01-01
;;
;;   # Actually delete (connects to IMAP, interactive prompts):
;;   racket purge-candidates.rkt --from noreply@github.com --delete
;;   racket purge-candidates.rkt --from noreply@github.com --delete -y
;;   racket purge-candidates.rkt --from noreply@github.com --before 2024-01-01 --delete
;;
;; The report mode (no --from) is purely local — it scans digests only.
;; The --from mode with --delete connects live to IMAP servers.
;;
;; Date filters (--year, --since, --before) apply in both modes:
;; in report mode they filter which messages are counted; in --from
;; mode they filter which messages are shown/deleted.

(require
  "src/imap-email-account-credentials.rkt"
  "src/connect-to-imap-account.rkt"
  "src/gmail-oauth2.rkt"
  "src/oauth2-details.rkt"
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  "src/known-contacts.rkt"
  "src/parse-mail-dates.rkt"
  net/imap
  net/head
  openssl
  gregor
  racket/serialize)

;; Handle broken pipe gracefully (e.g. when piping to head)
(void (with-handlers ([exn:fail? void])
        (file-stream-buffer-mode (current-output-port) 'line)))

;; ---- date helpers ----

;; Fast year extraction via regex — avoids expensive full date parsing.
;; Works for the vast majority of email date formats:
;;   "Thu, 15 Mar 2024 10:30:00 +0100"  -> 2024
;;   "2024-03-15T10:30:00+01:00"        -> 2024
;;   "15 Mar 2024 10:30:00 +0100"       -> 2024
(define year-rx #px"((?:19|20)[0-9]{2})")

(define (fast-extract-year date-string)
  (let ([m (regexp-match year-rx date-string)])
    (and m (string->number (cadr m)))))

(define (message-date hdr)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (possible-parse-date-time-string (main-mail-header-parts-date-string hdr))))

(define (parse-date-arg s)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (printf "Could not parse date ~s. Use format YYYY-MM-DD.~n" s)
                     (exit 1))])
    (parse-date s "yyyy-MM-dd")))

;; Fast date check using pre-computed fields from digest when available.
;; Falls back to regex/parsing for digests without parsed fields.
(define (date-matches? hdr year-filter since-filter before-filter)
  (cond
    [(and (not year-filter) (not since-filter) (not before-filter)) #t]
    ;; Year-only filter: use struct field if available, else fast regex
    [(and year-filter (not since-filter) (not before-filter))
     (let ([yr (or (main-mail-header-parts-parsed-year hdr)
                   (fast-extract-year (main-mail-header-parts-date-string hdr)))])
       (and yr (= yr year-filter)))]
    ;; Date range: use epoch if available, else full parsing
    [else
     (let ([epoch (main-mail-header-parts-parsed-epoch hdr)])
       (if epoch
           ;; Fast path: compare epoch seconds
           (and (or (not year-filter)
                    (let ([yr (main-mail-header-parts-parsed-year hdr)])
                      (and yr (= yr year-filter))))
                (or (not since-filter)
                    (>= epoch (->posix (datetime (->year since-filter) (->month since-filter) (->day since-filter)))))
                (or (not before-filter)
                    (< epoch (->posix (datetime (->year before-filter) (->month before-filter) (->day before-filter))))))
           ;; Slow path: full date parsing
           (let ([d (message-date hdr)])
             (if (not d)
                 #f
                 (let ([msg-date (->date d)])
                   (and (or (not year-filter)
                            (= (->year d) year-filter))
                        (or (not since-filter)
                            (date>=? msg-date since-filter))
                        (or (not before-filter)
                            (date<? msg-date before-filter))))))))]))

;; ---- from-address extraction ----

(define from-addr-rx #rx"<([^>]+)>")

(define (extract-from-addr from-str)
  (let ([m (regexp-match from-addr-rx from-str)])
    (if m
        (string-downcase (cadr m))
        (string-downcase (string-trim from-str)))))

;; ---- digest loading ----

(define (load-all-latest-digests)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (let ([by-key (make-hash)])
          (for ([f (directory-list dir #:build? #t)]
                #:when (regexp-match? #rx"\\.ser$" (path->string f)))
            (with-handlers ([exn:fail? (lambda (e) (void))])
              (let* ([mbd (load-mailbox-digest-from-file f)]
                     [key (cons (mailbox-digest-mail-address mbd)
                                (mailbox-digest-folder-name mbd))])
                (let ([existing (hash-ref by-key key #f)])
                  (when (or (not existing)
                            (datetime>? (mailbox-digest-timestamp mbd)
                                        (mailbox-digest-timestamp existing)))
                    (hash-set! by-key key mbd))))))
          (hash-values by-key))
        '())))

(define sent-folder-rx #rx"(?i:sent|gesendet|envoy|inviati|enviados|verzonden)")

(define (inbox-digests digests)
  (filter (lambda (mbd)
            (not (regexp-match? sent-folder-rx (mailbox-digest-folder-name mbd))))
          digests))

;; ---- report mode: list unknown senders by count ----

(define (report-purge-candidates digests known-set
                                 year-filter since-filter before-filter
                                 min-count)
  (let ([sender-counts (make-hash)]
        [sender-accounts (make-hash)]
        [total-unknown 0]
        [total-known 0]
        [total-scanned 0]
        [total-matched 0])

    ;; Count messages per sender
    (for ([mbd (inbox-digests digests)])
      (let ([account (mailbox-digest-mail-address mbd)])
        (for ([hdr (mailbox-digest-mail-headers mbd)])
          (set! total-scanned (add1 total-scanned))
          (when (date-matches? hdr year-filter since-filter before-filter)
            (set! total-matched (add1 total-matched))
            (let ([from (extract-from-addr (main-mail-header-parts-from hdr))])
              (if (set-member? known-set from)
                  (set! total-known (add1 total-known))
                  (begin
                    (set! total-unknown (add1 total-unknown))
                    (hash-update! sender-counts from add1 0)
                    (hash-update! sender-accounts from
                                 (lambda (s) (set-add s account))
                                 (set)))))))))

    ;; Sort by count descending
    (let ([sorted (sort (hash->list sender-counts) > #:key cdr)])
      (let ([filtered (filter (lambda (p) (>= (cdr p) min-count)) sorted)])

        (printf "~nPurge candidates (not in known-contacts):~n")
        (printf "~a messages scanned" total-scanned)
        (when (or year-filter since-filter before-filter)
          (printf ", ~a matched date filter" total-matched))
        (printf "~n")
        (printf "~a unknown senders, ~a messages (~a from known contacts excluded)~n~n"
                (hash-count sender-counts) total-unknown total-known)

        (when year-filter
          (printf "  (filtered to year ~a)~n" year-filter))
        (when since-filter
          (printf "  (filtered to since ~a)~n" (~t since-filter "yyyy-MM-dd")))
        (when before-filter
          (printf "  (filtered to before ~a)~n" (~t before-filter "yyyy-MM-dd")))
        (when (> min-count 1)
          (printf "  (showing senders with ~a+ messages)~n" min-count))
        (newline)

        (printf "  ~a  ~a  ~a~n"
                (~a "Count" #:min-width 7 #:align 'right)
                (~a "Accts" #:min-width 5 #:align 'right)
                "Sender")
        (printf "  ~a  ~a  ~a~n"
                (make-string 7 #\-)
                (make-string 5 #\-)
                (make-string 40 #\-))

        (for ([pair filtered])
          (let* ([sender (car pair)]
                 [count (cdr pair)]
                 [acct-count (set-count (hash-ref sender-accounts sender))])
            (printf "  ~a  ~a  ~a~n"
                    (~a count #:min-width 7 #:align 'right)
                    (~a acct-count #:min-width 5 #:align 'right)
                    sender)))

        (printf "~n  ~a senders shown (~a total messages)~n"
                (length filtered)
                (for/sum ([p filtered]) (cdr p)))))))

;; ---- from-address mode: show details for one sender ----

(define (show-from-details digests target-from
                           year-filter since-filter before-filter)
  (let ([total 0])
    (for ([mbd (sort (inbox-digests digests) string<?
                     #:key (lambda (d) (format "~a/~a"
                                               (mailbox-digest-mail-address d)
                                               (mailbox-digest-folder-name d))))])
      (let ([account (mailbox-digest-mail-address mbd)]
            [folder (mailbox-digest-folder-name mbd)]
            [matches '()])
        (for ([hdr (mailbox-digest-mail-headers mbd)])
          (let ([from (extract-from-addr (main-mail-header-parts-from hdr))])
            (when (and (string=? from target-from)
                       (date-matches? hdr year-filter since-filter before-filter))
              (set! matches
                    (cons (list (main-mail-header-parts-mail-id hdr)
                                (main-mail-header-parts-date-string hdr)
                                (main-mail-header-parts-subj hdr))
                          matches)))))
        (when (not (null? matches))
          (set! total (+ total (length matches)))
          (printf "~n~a / ~a: ~a message(s)~n" account folder (length matches))
          (for ([match (reverse matches)])
            (let ([uid (first match)]
                  [date (second match)]
                  [subj (third match)])
              (printf "  UID ~a  ~a  ~a~n"
                      uid date
                      (if (string=? subj "") "(no subject)" subj)))))))
    (printf "~n~a total message(s) from ~a~n" total target-from)
    total))

;; ---- delete mode: connect and delete for one sender ----

(define (connect-to credential folder-name)
  (if (imap-email-account-credentials-xoauth2? credential)
      (let ([oauth2-creds (load-google-oauth2-details)]
            [email (imap-email-account-credentials-mailaddress credential)])
        (oauth2-connect-to-imap email oauth2-creds folder-name))
      (securely-connect-to-imap-account credential folder-name)))

(define (load-credentials)
  (read-email-account-credentials-hash-from-file-named
   (default-credentials-filepath)))

(define (email->credential creds email)
  (for/first ([name (hash-keys creds)]
              #:when (string=? (imap-email-account-credentials-mailaddress
                                (hash-ref creds name))
                               email))
    (hash-ref creds name)))

;; Mark UIDs in a digest with $DeletedOnIMAPServer and re-save.
(define (mark-deleted-in-digest email folder-name deleted-uids)
  (let ([digest-path (find-latest-digest-for email folder-name)])
    (when digest-path
      (let* ([mbd (load-mailbox-digest-from-file digest-path)]
             [uid-set (list->set deleted-uids)]
             [updated-headers
              (for/list ([hdr (mailbox-digest-mail-headers mbd)])
                (if (set-member? uid-set (main-mail-header-parts-mail-id hdr))
                    (let ([new-flags
                           (if (member '|$DeletedOnIMAPServer|
                                       (main-mail-header-parts-flags hdr))
                               (main-mail-header-parts-flags hdr)
                               (cons '|$DeletedOnIMAPServer|
                                     (main-mail-header-parts-flags hdr)))])
                      (main-mail-header-parts
                       (main-mail-header-parts-mail-id hdr)
                       (main-mail-header-parts-date-string hdr)
                       (main-mail-header-parts-from hdr)
                       (main-mail-header-parts-to hdr)
                       (main-mail-header-parts-cc hdr)
                       (main-mail-header-parts-bcc hdr)
                       (main-mail-header-parts-subj hdr)
                       new-flags
                       (main-mail-header-parts-parsed-year hdr)
                       (main-mail-header-parts-parsed-epoch hdr)))
                    hdr))]
             [updated-digest
              (mailbox-digest
               (mailbox-digest-mail-address mbd)
               (mailbox-digest-folder-name mbd)
               (mailbox-digest-uid-validity mbd)
               (mailbox-digest-index-range mbd)
               updated-headers
               (mailbox-digest-timestamp mbd))])
        (call-with-output-file digest-path
          (lambda (out) (write (serialize updated-digest) out))
          #:exists 'replace)
        (printf "  Marked ~a message(s) as $DeletedOnIMAPServer in local digest.~n"
                (length deleted-uids))))))

;; Find sequence numbers for messages from a specific sender,
;; optionally filtered by date.
(define (find-seqnos-from-sender imap-conn target-from msg-count
                                  year-filter since-filter before-filter)
  (let ([batch-size 200]
        [matches '()])
    (let loop ([start 1])
      (if (> start msg-count)
          (reverse matches)
          (let* ([end (min msg-count (+ start batch-size -1))]
                 [indices (for/list ([i (in-range start (+ end 1))]) i)]
                 [results (imap-get-messages imap-conn indices '(uid header flags))])
            (for ([result results]
                  [seqno (in-range start (+ end 1))])
              (let ([uid (first result)]
                    [header (second result)]
                    [flags (third result)])
                (with-handlers ([exn:fail? (lambda (e) (void))])
                  (let* ([from-raw (bytes->string/utf-8
                                    (extract-field #"from" header))]
                         [from (extract-from-addr from-raw)])
                    (when (string=? from target-from)
                      ;; Check date filter against header date
                      (let ([date-raw
                             (with-handlers ([exn:fail? (lambda (e) "")])
                               (bytes->string/utf-8
                                (extract-field #"date" header)))])
                        (let ([date-ok?
                               (cond
                                 [(and (not year-filter) (not since-filter)
                                       (not before-filter))
                                  #t]
                                 [else
                                  (let ([d (with-handlers ([exn:fail? (lambda (e) #f)])
                                             (possible-parse-date-time-string date-raw))])
                                    (cond
                                      [(not d) #f]
                                      [year-filter (= (->year d) year-filter)]
                                      [else
                                       (let ([msg-date (->date d)])
                                         (and (or (not since-filter)
                                                  (date>=? msg-date since-filter))
                                              (or (not before-filter)
                                                  (date<? msg-date before-filter))))]))])])
                          (when date-ok?
                            (let ([subj (with-handlers ([exn:fail? (lambda (e) "")])
                                          (bytes->string/utf-8
                                           (extract-field #"subject" header)))])
                              (set! matches
                                    (cons (list seqno uid from-raw subj date-raw flags)
                                          matches)))))))))))
            (when (= (modulo start 1000) 0)
              (printf "  ...scanned ~a of ~a~n" start msg-count))
            (loop (+ end 1)))))))

(define (delete-from-sender-in-folder credential email folder-name target-from
                                       year-filter since-filter before-filter
                                       auto-confirm?)
  (let ([account-name (imap-email-account-credentials-accountname credential)])
    (printf "~n========================================~n")
    (printf "  ~a (~a) / ~a~n" account-name email folder-name)
    (printf "========================================~n")
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (printf "ERROR: ~a~n" (exn-message e)))])
      (let* ([imap-conn (connect-to credential folder-name)]
             [msg-count (imap-messages imap-conn)])

        (printf "Scanning ~a messages for mail from ~a...~n" msg-count target-from)
        (let ([matches (find-seqnos-from-sender
                        imap-conn target-from msg-count
                        year-filter since-filter before-filter)])
          (if (null? matches)
              (printf "No matching messages found.~n")
              (begin
                (printf "Found ~a message(s):~n~n" (length matches))
                (for ([m matches])
                  (let ([seqno (first m)]
                        [uid (second m)]
                        [from (third m)]
                        [subj (fourth m)]
                        [date (fifth m)]
                        [flags (sixth m)])
                    (printf "  UID ~a  ~a  ~a~n" uid date
                            (if (string=? subj "") "(no subject)" subj))))

                (printf "~n")
                (if auto-confirm?
                    (begin
                      (printf "Deleting ~a message(s)...~n" (length matches))
                      (let ([seqnos (map first matches)])
                        (imap-store imap-conn '+ (sort seqnos >)
                                    (list (symbol->imap-flag 'deleted)))
                        (imap-expunge imap-conn))
                      (printf "  Deleted and expunged ~a message(s).~n" (length matches))
                      (mark-deleted-in-digest email folder-name
                                              (map second matches)))
                    (begin
                      (printf "Delete all ~a message(s) from ~a? [y/N] "
                              (length matches) target-from)
                      (flush-output)
                      (let ([answer (read-line)])
                        (if (and answer (regexp-match? #rx"^[yY]" answer))
                            (begin
                              (let ([seqnos (map first matches)])
                                (imap-store imap-conn '+ (sort seqnos >)
                                            (list (symbol->imap-flag 'deleted)))
                                (imap-expunge imap-conn))
                              (printf "  Deleted and expunged ~a message(s).~n"
                                      (length matches))
                              (mark-deleted-in-digest email folder-name
                                                      (map second matches)))
                            (printf "Not deleted.~n"))))))))

        (imap-disconnect imap-conn)))))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)]
        [from-filter #f]
        [year-filter #f]
        [since-filter #f]
        [before-filter #f]
        [min-count 2]
        [delete? #f]
        [auto-confirm? #f])
    (let loop ([remaining arg-list])
      (cond
        [(null? remaining) (void)]
        [(and (string=? (car remaining) "--from")
              (not (null? (cdr remaining))))
         (set! from-filter (string-downcase (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--year")
              (not (null? (cdr remaining))))
         (set! year-filter (string->number (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--since")
              (not (null? (cdr remaining))))
         (set! since-filter (parse-date-arg (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--before")
              (not (null? (cdr remaining))))
         (set! before-filter (parse-date-arg (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--min")
              (not (null? (cdr remaining))))
         (set! min-count (string->number (cadr remaining)))
         (loop (cddr remaining))]
        [(string=? (car remaining) "--delete")
         (set! delete? #t)
         (loop (cdr remaining))]
        [(or (string=? (car remaining) "--yes")
             (string=? (car remaining) "-y"))
         (set! auto-confirm? #t)
         (loop (cdr remaining))]
        [else (loop (cdr remaining))]))
    (values from-filter year-filter since-filter before-filter
            min-count delete? auto-confirm?)))

;; ---- main ----

(define (main)
  (let-values ([(from-filter year-filter since-filter before-filter
                 min-count delete? auto-confirm?)
                (parse-args (current-command-line-arguments))])

    (let ([known-set (load-known-contacts (default-known-contacts-filepath))]
          [digests (load-all-latest-digests)])

      (when (null? digests)
        (printf "No digests found.~n")
        (exit 0))

      (cond
        ;; Mode 1: Report — list unknown senders by message count
        [(not from-filter)
         (report-purge-candidates digests known-set
                                  year-filter since-filter before-filter
                                  min-count)]

        ;; Mode 2: Show details for a specific sender
        [(and from-filter (not delete?))
         (printf "Messages from ~a:~n" from-filter)
         (show-from-details digests from-filter
                            year-filter since-filter before-filter)]

        ;; Mode 3: Delete messages from a specific sender
        [(and from-filter delete?)
         ;; First show what we'll delete from digest data
         (printf "Messages from ~a to delete:~n" from-filter)
         (let ([total (show-from-details digests from-filter
                                         year-filter since-filter before-filter)])
           (when (> total 0)
             (printf "~nConnecting to IMAP to delete...~n")

             ;; Find which accounts+folders have messages from this sender
             (let ([creds (load-credentials)])
               (for ([mbd (inbox-digests digests)])
                 (let ([email (mailbox-digest-mail-address mbd)]
                       [folder (mailbox-digest-folder-name mbd)]
                       [has-match? #f])
                   (for ([hdr (mailbox-digest-mail-headers mbd)]
                         #:break has-match?)
                     (let ([from (extract-from-addr
                                  (main-mail-header-parts-from hdr))])
                       (when (and (string=? from from-filter)
                                  (date-matches? hdr year-filter
                                                 since-filter before-filter))
                         (set! has-match? #t))))
                   (when has-match?
                     (let ([credential (email->credential creds email)])
                       (if credential
                           (delete-from-sender-in-folder
                            credential email folder from-filter
                            year-filter since-filter before-filter
                            auto-confirm?)
                           (printf "WARNING: no credential found for ~a~n"
                                   email)))))))))]))))

(main)
