#lang racket

;; Find purge candidates: bulk senders not in your known-contacts.
;;
;; Usage:
;;   # List senders not in known-contacts, sorted by message count:
;;   racket purge-candidates.rkt
;;   racket purge-candidates.rkt --min 50          ; only senders with 50+ messages
;;   racket purge-candidates.rkt --before 2023-01-01  ; only count old messages
;;   racket purge-candidates.rkt --after 2019-01-01 --before 2020-01-01  ; one year
;;   racket purge-candidates.rkt --year 2020       ; only count messages from 2020
;;   racket purge-candidates.rkt --sort size       ; sort by total size instead of count
;;   racket purge-candidates.rkt --account tbh361  ; only this account (substring match)
;;
;;   # Show what would be deleted for a specific sender:
;;   racket purge-candidates.rkt --from noreply@github.com
;;   racket purge-candidates.rkt --from noreply@github.com --before 2024-01-01
;;
;;   # Delete a single sender (connects to IMAP, interactive prompts):
;;   racket purge-candidates.rkt --from noreply@github.com --delete
;;   racket purge-candidates.rkt --from noreply@github.com --delete -y
;;   racket purge-candidates.rkt --from noreply@github.com --before 2024-01-01 --delete
;;
;;   # Batch delete ALL unknown senders matching filters:
;;   racket purge-candidates.rkt --account tbh361 --min 100 --delete-all -y
;;   racket purge-candidates.rkt --account tbh361 --min 50 --after 2019-01-01 --before 2020-01-01 --delete-all -y
;;   racket purge-candidates.rkt --account tbh361 --min 10 --delete-all --keep 2 -y
;;
;; The report mode (no --from/--delete-all) is purely local — it scans digests only.
;; The --from --delete and --delete-all modes connect live to IMAP servers.
;;
;; Date filters (--year, --after/--since, --before) apply in all modes.
;; --keep N retains the N newest messages per sender (by epoch/date).

(require
  "src/imap-email-account-credentials.rkt"
  "src/connect-to-imap-account.rkt"
  "src/gmail-oauth2.rkt"
  "src/oauth2-details.rkt"
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  "src/known-contacts.rkt"
  "src/parse-mail-dates.rkt"
  "src/utils.rkt"
  net/imap
  net/head
  openssl
  gregor
  racket/serialize)

;; Handle broken pipe gracefully (e.g. when piping to head)
(handle-broken-pipe)

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

;; ---- report mode: list unknown senders by count ----

(define (report-purge-candidates digests known-set
                                 year-filter since-filter before-filter
                                 min-count sort-by)
  (let ([sender-counts (make-hash)]
        [sender-sizes (make-hash)]
        [sender-accounts (make-hash)]
        [total-unknown 0]
        [total-known 0]
        [total-scanned 0]
        [total-matched 0]
        [has-sizes? #f])

    ;; Count messages per sender (skip tombstoned messages)
    (for ([mbd (non-sent-digests digests)])
      (let ([account (mailbox-digest-mail-address mbd)])
        (for ([hdr (mailbox-digest-mail-headers mbd)])
          (unless (main-mail-header-parts-deleted? hdr)
            (set! total-scanned (add1 total-scanned))
            (when (date-matches? hdr year-filter since-filter before-filter)
              (set! total-matched (add1 total-matched))
              (let ([from (extract-from-addr (main-mail-header-parts-from hdr))]
                    [sz (main-mail-header-parts-message-size hdr)])
                (when sz (set! has-sizes? #t))
                (if (set-member? known-set from)
                    (set! total-known (add1 total-known))
                    (begin
                      (set! total-unknown (add1 total-unknown))
                      (hash-update! sender-counts from add1 0)
                      (when sz
                        (hash-update! sender-sizes from
                                     (lambda (v) (+ v sz)) 0))
                      (hash-update! sender-accounts from
                                   (lambda (s) (set-add s account))
                                   (set))))))))))
    (let ([sorted (if (eq? sort-by 'size)
                      (sort (hash->list sender-counts) >
                            #:key (lambda (p) (hash-ref sender-sizes (car p) 0)))
                      (sort (hash->list sender-counts) > #:key cdr))])
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
          (printf "  (filtered to after ~a)~n" (~t since-filter "yyyy-MM-dd")))
        (when before-filter
          (printf "  (filtered to before ~a)~n" (~t before-filter "yyyy-MM-dd")))
        (when (> min-count 1)
          (printf "  (showing senders with ~a+ messages)~n" min-count))
        (newline)

        (if has-sizes?
            (begin
              (printf "  ~a  ~a  ~a  ~a~n"
                      (~a "Count" #:min-width 7 #:align 'right)
                      (~a "Size" #:min-width 10 #:align 'right)
                      (~a "Accts" #:min-width 5 #:align 'right)
                      "Sender")
              (printf "  ~a  ~a  ~a  ~a~n"
                      (make-string 7 #\-)
                      (make-string 10 #\-)
                      (make-string 5 #\-)
                      (make-string 40 #\-)))
            (begin
              (printf "  ~a  ~a  ~a~n"
                      (~a "Count" #:min-width 7 #:align 'right)
                      (~a "Accts" #:min-width 5 #:align 'right)
                      "Sender")
              (printf "  ~a  ~a  ~a~n"
                      (make-string 7 #\-)
                      (make-string 5 #\-)
                      (make-string 40 #\-))))

        (for ([pair filtered])
          (let* ([sender (car pair)]
                 [count (cdr pair)]
                 [size (hash-ref sender-sizes sender 0)]
                 [acct-count (set-count (hash-ref sender-accounts sender))])
            (if has-sizes?
                (printf "  ~a  ~a  ~a  ~a~n"
                        (~a count #:min-width 7 #:align 'right)
                        (~a (format-size size) #:min-width 10 #:align 'right)
                        (~a acct-count #:min-width 5 #:align 'right)
                        sender)
                (printf "  ~a  ~a  ~a~n"
                        (~a count #:min-width 7 #:align 'right)
                        (~a acct-count #:min-width 5 #:align 'right)
                        sender))))

        (let ([total-msgs (for/sum ([p filtered]) (cdr p))]
              [total-sz (for/sum ([p filtered]) (hash-ref sender-sizes (car p) 0))])
          (if has-sizes?
              (printf "~n  ~a senders shown (~a total messages, ~a)~n"
                      (length filtered) total-msgs (format-size total-sz))
              (printf "~n  ~a senders shown (~a total messages)~n"
                      (length filtered) total-msgs)))))))

;; ---- from-address mode: show details for one sender ----

(define (show-from-details digests target-from
                           year-filter since-filter before-filter)
  (let ([total 0])
    (for ([mbd (sort (non-sent-digests digests) string<?
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
                       (main-mail-header-parts-parsed-epoch hdr)
                       (main-mail-header-parts-message-size hdr)))
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

;; ---- delete-all mode: batch purge across all matching senders ----

;; Collect all unknown senders matching filters from digests.
;; Returns a list of (sender . count) pairs, sorted by sort-by.
(define (collect-purge-senders digests known-set
                                year-filter since-filter before-filter
                                min-count sort-by)
  (let ([sender-counts (make-hash)]
        [sender-sizes (make-hash)])
    (for ([mbd (non-sent-digests digests)])
      (for ([hdr (mailbox-digest-mail-headers mbd)])
        (unless (main-mail-header-parts-deleted? hdr)
          (when (date-matches? hdr year-filter since-filter before-filter)
            (let ([from (extract-from-addr (main-mail-header-parts-from hdr))]
                  [sz (main-mail-header-parts-message-size hdr)])
              (unless (set-member? known-set from)
                (hash-update! sender-counts from add1 0)
                (when sz
                  (hash-update! sender-sizes from (lambda (v) (+ v sz)) 0))))))))
    (let* ([all-pairs (hash->list sender-counts)]
           [filtered (filter (lambda (p) (>= (cdr p) min-count)) all-pairs)]
           [sorted (if (eq? sort-by 'size)
                       (sort filtered >
                             #:key (lambda (p) (hash-ref sender-sizes (car p) 0)))
                       (sort filtered > #:key cdr))])
      (values sorted sender-sizes))))

;; For --keep N: collect UIDs for a sender from digests, sorted newest first.
;; Returns list of (uid . epoch) pairs for the given sender matching date filters.
(define (sender-uids-by-recency digests target-from
                                 year-filter since-filter before-filter)
  (let ([uid-epochs '()])
    (for ([mbd (non-sent-digests digests)])
      (for ([hdr (mailbox-digest-mail-headers mbd)])
        (let ([from (extract-from-addr (main-mail-header-parts-from hdr))])
          (when (and (string=? from target-from)
                     (date-matches? hdr year-filter since-filter before-filter))
            (let ([uid (main-mail-header-parts-mail-id hdr)]
                  [epoch (or (main-mail-header-parts-parsed-epoch hdr) 0)]
                  [email (mailbox-digest-mail-address mbd)]
                  [folder (mailbox-digest-folder-name mbd)])
              (set! uid-epochs
                    (cons (list uid epoch email folder) uid-epochs)))))))
    ;; Sort newest first
    (sort uid-epochs > #:key second)))

;; Find sequence numbers on IMAP for specific UIDs.
;; Returns list of (seqno . uid) pairs for UIDs that exist.
(define (find-seqnos-for-uids imap-conn target-uids msg-count)
  (let ([uid-set (list->set target-uids)]
        [batch-size 200]
        [matches '()])
    (let loop ([start 1])
      (if (> start msg-count)
          (reverse matches)
          (let* ([end (min msg-count (+ start batch-size -1))]
                 [indices (for/list ([i (in-range start (+ end 1))]) i)]
                 [results (imap-get-messages imap-conn indices '(uid))])
            (for ([result results]
                  [seqno (in-range start (+ end 1))])
              (let ([uid (first result)])
                (when (set-member? uid-set uid)
                  (set! matches (cons (cons seqno uid) matches)))))
            (loop (+ end 1)))))))

;; Execute batch deletion for all matching senders.
(define (execute-delete-all digests known-set
                             year-filter since-filter before-filter
                             min-count sort-by keep-count keep-per
                             auto-confirm? dry-run?)
  (let-values ([(senders sender-sizes)
                (collect-purge-senders digests known-set
                                       year-filter since-filter before-filter
                                       min-count sort-by)])
    (when (null? senders)
      (printf "No senders match the filters.~n")
      (exit 0))

    ;; Show what we're about to do
    (let ([total-msgs (for/sum ([p senders]) (cdr p))]
          [total-sz (for/sum ([p senders]) (hash-ref sender-sizes (car p) 0))])
      (printf "~nBatch delete plan~a:~n"
              (if dry-run? " (DRY RUN)" ""))
      (printf "  ~a senders, ~a messages" (length senders) total-msgs)
      (when (> total-sz 0) (printf ", ~a" (format-size total-sz)))
      (newline)
      (when keep-count
        (printf "  Keeping ~a newest message(s) per sender per ~a~n"
                keep-count keep-per))
      (printf "~n")

      ;; Show first few senders
      (let ([preview (take senders (min 10 (length senders)))])
        (for ([p preview])
          (let ([sz (hash-ref sender-sizes (car p) 0)])
            (printf "  ~a  ~a  ~a~n"
                    (~a (cdr p) #:min-width 7 #:align 'right)
                    (~a (if (> sz 0) (format-size sz) "-") #:min-width 10 #:align 'right)
                    (car p))))
        (when (> (length senders) 10)
          (printf "  ... and ~a more senders~n" (- (length senders) 10))))

      ;; In dry run, stop here
      (when dry-run?
        (printf "~n(dry run — no deletions performed)~n")
        (exit 0))

      ;; Confirm
      (unless auto-confirm?
        (printf "~nProceed with deletion? [y/N] ")
        (flush-output)
        (let ([answer (read-line)])
          (unless (and answer (regexp-match? #rx"^[yY]" answer))
            (printf "Aborted.~n")
            (exit 0)))))

    ;; Load credentials
    (let ([creds (load-credentials)])

      ;; Group work by account+folder to minimize IMAP connections
      ;; Build a hash: (email . folder) -> list of (sender uid epoch)
      (let ([work-by-folder (make-hash)]
            [sender-set (list->set (map car senders))])

        ;; Collect all UIDs to delete, respecting --keep
        (for ([mbd (non-sent-digests digests)])
          (let ([email (mailbox-digest-mail-address mbd)]
                [folder (mailbox-digest-folder-name mbd)])
            (let ([sender-msgs (make-hash)])  ;; sender -> list of (uid epoch)
              ;; Collect messages per sender for this folder
              (for ([hdr (mailbox-digest-mail-headers mbd)])
                (unless (main-mail-header-parts-deleted? hdr)
                  (let ([from (extract-from-addr (main-mail-header-parts-from hdr))])
                    (when (and (set-member? sender-set from)
                               (date-matches? hdr year-filter since-filter before-filter))
                      (let ([uid (main-mail-header-parts-mail-id hdr)]
                            [epoch (or (main-mail-header-parts-parsed-epoch hdr) 0)])
                        (hash-update! sender-msgs from
                                      (lambda (lst) (cons (list uid epoch) lst))
                                      '()))))))

              ;; For each sender, group messages by period (year or month
               ;; bucket key derived from epoch), then within each bucket sort
               ;; newest first and drop the first `keep-count` to retain them.
              (for ([(sender msgs) (in-hash sender-msgs)])
                (let* ([buckets (make-hash)])
                  (for ([m msgs])
                    (let* ([epoch (second m)]
                           [bucket-key
                            (cond
                              [(or (not keep-count) (eq? keep-per 'all)) 'all]
                              [(= epoch 0) 'unknown]  ;; no parseable date
                              [else
                               (let ([dt (posix->datetime epoch)])
                                 (case keep-per
                                   [(month) (format "~a-~a"
                                                    (->year dt)
                                                    (~r (->month dt)
                                                        #:min-width 2
                                                        #:pad-string "0"))]
                                   [(year) (->year dt)]
                                   [else 'all]))])])
                      (hash-update! buckets bucket-key
                                    (lambda (lst) (cons m lst))
                                    '())))
                  ;; Within each bucket: sort newest first, keep N, delete rest
                  (for ([(bucket bmsgs) (in-hash buckets)])
                    (let* ([sorted-msgs (sort bmsgs > #:key second)]
                           [to-delete (if keep-count
                                          (drop sorted-msgs
                                                (min keep-count
                                                     (length sorted-msgs)))
                                          sorted-msgs)])
                      (when (not (null? to-delete))
                        (let ([key (cons email folder)])
                          (hash-update! work-by-folder key
                                        (lambda (lst)
                                          (append (map first to-delete) lst))
                                        '()))))))))))

        ;; Now connect to each account+folder and delete
        (let ([total-deleted 0])
          (for ([(key uids) (in-hash work-by-folder)])
            (let ([email (car key)]
                  [folder (cdr key)])
              (printf "~n~a / ~a: ~a messages to delete~n" email folder (length uids))
              (let ([credential (email->credential creds email)])
                (if (not credential)
                    (printf "  WARNING: no credential found for ~a, skipping.~n" email)
                    (with-handlers
                        ([exn:fail?
                          (lambda (e)
                            (printf "  ERROR: ~a~n" (exn-message e)))])
                      (let* ([imap-conn (connect-to credential folder)]
                             [msg-count (imap-messages imap-conn)]
                             [uid-seqno-pairs (find-seqnos-for-uids
                                               imap-conn uids msg-count)])
                        (if (null? uid-seqno-pairs)
                            (printf "  No matching messages found on server.~n")
                            (begin
                              (printf "  Found ~a message(s) on server, deleting...~n"
                                      (length uid-seqno-pairs))
                              (let ([seqnos (map car uid-seqno-pairs)])
                                (imap-store imap-conn '+ (sort seqnos >)
                                            (list (symbol->imap-flag 'deleted)))
                                (imap-expunge imap-conn))
                              (set! total-deleted (+ total-deleted
                                                     (length uid-seqno-pairs)))
                              (printf "  Deleted and expunged ~a message(s).~n"
                                      (length uid-seqno-pairs))
                              (mark-deleted-in-digest email folder
                                                      (map cdr uid-seqno-pairs))))
                        (imap-disconnect imap-conn)))))))
          (printf "~n========================================~n")
          (printf "Total deleted: ~a messages~n" total-deleted)
          (printf "========================================~n"))))))

;; ---- arg parsing ----

(define (parse-args args)
  (let ([arg-list (vector->list args)]
        [from-filter #f]
        [year-filter #f]
        [since-filter #f]
        [before-filter #f]
        [min-count 2]
        [delete? #f]
        [delete-all? #f]
        [auto-confirm? #f]
        [sort-by 'count]
        [account-filter #f]
        [keep-count #f]
        [keep-per 'all]
        [dry-run? #f])
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
        [(and (or (string=? (car remaining) "--since")
                  (string=? (car remaining) "--after"))
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
        [(and (string=? (car remaining) "--sort")
              (not (null? (cdr remaining))))
         (set! sort-by (string->symbol (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--account")
              (not (null? (cdr remaining))))
         (set! account-filter (string-downcase (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--keep")
              (not (null? (cdr remaining))))
         (set! keep-count (string->number (cadr remaining)))
         (loop (cddr remaining))]
        [(and (string=? (car remaining) "--keep-per")
              (not (null? (cdr remaining))))
         (set! keep-per (string->symbol (cadr remaining)))
         (loop (cddr remaining))]
        [(string=? (car remaining) "--delete")
         (set! delete? #t)
         (loop (cdr remaining))]
        [(string=? (car remaining) "--delete-all")
         (set! delete-all? #t)
         (loop (cdr remaining))]
        [(or (string=? (car remaining) "--yes")
             (string=? (car remaining) "-y"))
         (set! auto-confirm? #t)
         (loop (cdr remaining))]
        [(or (string=? (car remaining) "--dry-run")
             (string=? (car remaining) "-n"))
         (set! dry-run? #t)
         (loop (cdr remaining))]
        [else (loop (cdr remaining))]))
    (values from-filter year-filter since-filter before-filter
            min-count delete? delete-all? auto-confirm? sort-by
            account-filter keep-count keep-per dry-run?)))

;; ---- main ----

(define (main)
  (let-values ([(from-filter year-filter since-filter before-filter
                 min-count delete? delete-all? auto-confirm? sort-by
                 account-filter keep-count keep-per dry-run?)
                (parse-args (current-command-line-arguments))])

    (let ([known-set (load-all-known-contacts)]
          [all-digests (load-all-latest-digests)])

      (when (null? all-digests)
        (printf "No digests found.~n")
        (exit 0))

      ;; Apply account filter if specified
      (let ([digests (if account-filter
                        (filter (lambda (mbd)
                                  (string-contains?
                                   (string-downcase (mailbox-digest-mail-address mbd))
                                   account-filter))
                                all-digests)
                        all-digests)])

        (when (and account-filter (null? digests))
          (printf "No digests found matching account '~a'.~n" account-filter)
          (exit 0))

        (when account-filter
          (printf "Filtered to account(s) matching '~a'~n" account-filter))

      (cond
        ;; Mode 4: Batch delete all matching senders
        [delete-all?
         (execute-delete-all digests known-set
                             year-filter since-filter before-filter
                             min-count sort-by keep-count keep-per
                             auto-confirm? dry-run?)]

        ;; Mode 1: Report — list unknown senders by message count
        [(not from-filter)
         (report-purge-candidates digests known-set
                                  year-filter since-filter before-filter
                                  min-count sort-by)]

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
               (for ([mbd (non-sent-digests digests)])
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
                                   email)))))))))])))))

(main)
