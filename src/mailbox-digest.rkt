#lang racket

(require
  "imap-email-account-credentials.rkt"
  "connect-to-imap-account.rkt"
  "gmail-oauth2.rkt"
  net/imap
  "main-mail-header-parts.rkt"
  "mail-digest.rkt"
  gregor
  racket/serialize
  )

;; A snapshot of mail headers from one folder of one account.
(struct mailbox-digest
  (mail-address folder-name uid-validity index-range mail-headers timestamp)
  #:prefab
  )

(provide
 (contract-out
  ;; struct automatics
  [mailbox-digest (-> string? string? integer? pair? list? datetime? mailbox-digest?)]
  [mailbox-digest? (-> any/c boolean?)]
  [mailbox-digest-mail-address (-> mailbox-digest? string?)]
  [mailbox-digest-folder-name (-> mailbox-digest? string?)]
  [mailbox-digest-uid-validity (-> mailbox-digest? integer?)]
  [mailbox-digest-index-range (-> mailbox-digest? pair?)]
  [mailbox-digest-mail-headers (-> mailbox-digest? list?)]
  [mailbox-digest-timestamp (-> mailbox-digest? datetime?)]

  ;; loading from IMAP
  [get-mailbox-digest (-> imap-email-account-credentials? string? pair? mailbox-digest?)]
  ;; saving to / loading from file
  [save-mailbox-digest (-> mailbox-digest? path? path?)]
  [load-mailbox-digest-from-file (-> path? mailbox-digest?)]
  ;; file support
  [default-digest-dir (-> path?)]
  [mail-digest-file-name (-> mailbox-digest? string?)]
  
  ;; analysis
  [mailbox-digest-count (-> mailbox-digest? integer?)]
  [mailbox-digest-mail-digests-by-uid (-> mailbox-digest? hash?)]
  [mailbox-digest-mailids-by-from-addr (-> mailbox-digest? hash?)]
  [mailbox-digest-max-uid (-> mailbox-digest? integer?)]

  ;; incremental support
  [find-latest-digest-for (-> string? string? (or/c path? #f))]
  [merge-mailbox-digests (-> mailbox-digest? mailbox-digest? mailbox-digest?)]
 ))


;; ---- IMAP connection (password or OAuth2) ----

(define (connect-to-account mail-account-credential folder-name)
  (if (imap-email-account-credentials-xoauth2? mail-account-credential)
      (let ([oauth2-creds (load-google-oauth2-details)]
            [email (imap-email-account-credentials-mailaddress mail-account-credential)])
        (oauth2-connect-to-imap email oauth2-creds folder-name))
      (securely-connect-to-imap-account mail-account-credential folder-name)))


;; ---- batched fetching with progress ----

(define BATCH-SIZE 200)

(define (fetch-headers-in-batches imap-conn indices)
  (let ([total (length indices)])
    (let loop ([remaining indices]
               [fetched-so-far '()]
               [done-count 0])
      (if (null? remaining)
          (reverse fetched-so-far)
          (let* ([batch (take remaining (min BATCH-SIZE (length remaining)))]
                 [rest (drop remaining (length batch))]
                 [batch-results
                  (map mail-header->main-mail-header-parts
                       (imap-get-messages imap-conn batch main-mail-header-part-imap-symbols))]
                 [new-done (+ done-count (length batch))])
            (printf "  fetched ~a / ~a (~a%)~n"
                    new-done total
                    (round (* 100.0 (/ new-done total))))
            (loop rest
                  (append (reverse batch-results) fetched-so-far)
                  new-done))))))


;; ---- main fetch ----

;; Fetch headers from an IMAP server and build a local digest.
;; Automatically uses OAuth2 for accounts with xoauth2? set to #t.
(define (get-mailbox-digest mail-account-credential folder-name item-index-range)
  (let ([imap-conn (connect-to-account mail-account-credential folder-name)]
        [now-timestamp (now)])
    (let ([msg-count (imap-messages imap-conn)]
          [uid-validity (imap-uidvalidity imap-conn)]
          [lo-index (car item-index-range)])
      (let ([hi-index (min msg-count (cdr item-index-range))])
        (printf "~a messages in ~a; examining ~a to ~a~n"
                msg-count folder-name lo-index hi-index)
        (let* ([indices (stream->list (in-range lo-index (+ hi-index 1)))]
               [range-of-message-headers (fetch-headers-in-batches imap-conn indices)])
          (imap-disconnect imap-conn)
          (mailbox-digest
           (imap-email-account-credentials-mailaddress mail-account-credential)
           folder-name
           uid-validity
           item-index-range
           range-of-message-headers
           now-timestamp))))))


;; ---- save / load ----

(define (save-mailbox-digest a-mailbox-digest output-dir)
  (let ([full-file-path
         (build-path output-dir (mail-digest-file-name a-mailbox-digest))])
    (call-with-output-file full-file-path
      (lambda (out) (write (serialize a-mailbox-digest) out)))
    full-file-path))

(define (load-mailbox-digest-from-file digest-file-path)
  (call-with-input-file digest-file-path
    (lambda (in) (deserialize (read in)))))

(define (default-digest-dir)
  (build-path (find-system-path 'home-dir) ".imap_secrets" "digests"))

(define (mail-digest-file-name a-mailbox-digest)
  (format "mailbox-digest_~a_~a_~a.ser"
          (~t (mailbox-digest-timestamp a-mailbox-digest)
              "yyyy-MM-dd_HH:mm:ss")
          (mailbox-digest-mail-address a-mailbox-digest)
          (mailbox-digest-folder-name a-mailbox-digest)))


;; ---- analysis ----

(define (mailbox-digest-count a-mailbox-digest)
  (length (mailbox-digest-mail-headers a-mailbox-digest)))

(define (mailbox-digest-mail-digests-by-uid a-mailbox-digest)
  (for/fold ([result (make-immutable-hash)])
            ([mail-header (mailbox-digest-mail-headers a-mailbox-digest)])
    (values (hash-set result (main-mail-header-parts-mail-id mail-header)
                      (mail-digest-from-header-parts mail-header)))))

(define (mailbox-digest-mailids-by-from-addr a-mailbox-digest)
  (for/fold ([result (make-immutable-hash)])
            ([mail-header (mailbox-digest-mail-headers a-mailbox-digest)])
    (let ([mail-digest (mail-digest-from-header-parts mail-header)]
          [mail-id (main-mail-header-parts-mail-id mail-header)])
      (let ([from-addr (mail-digest 'from-addr)])
        (values (hash-update result
                             from-addr
                             (lambda (uid-set) (set-add uid-set mail-id))
                             (set)))))))

;; Highest UID in a digest (useful for incremental fetch).
(define (mailbox-digest-max-uid a-mailbox-digest)
  (for/fold ([max-uid 0])
            ([hdr (mailbox-digest-mail-headers a-mailbox-digest)])
    (values (max max-uid (main-mail-header-parts-mail-id hdr)))))


;; ---- incremental fetch support ----

;; Find the most recent saved digest file for a given email+folder.
;; Returns a path or #f.
(define (find-latest-digest-for email-address folder-name)
  (let ([dir (default-digest-dir)])
    (if (directory-exists? dir)
        (let* ([suffix (format "_~a_~a.ser" email-address folder-name)]
               [matching
                (sort
                 (for/list ([f (directory-list dir #:build? #t)]
                            #:when (string-suffix? (path->string (file-name-from-path f))
                                                   suffix))
                   f)
                 string>?
                 #:key path->string)])
          (if (null? matching) #f (first matching)))
        #f)))

;; Merge two digests for the same account+folder.
;; Deduplicates by UID, keeping all unique messages.
;; The resulting digest gets the newer timestamp and the wider index range.
(define (merge-mailbox-digests old-digest new-digest)
  (let ([seen-uids (for/set ([hdr (mailbox-digest-mail-headers old-digest)])
                     (main-mail-header-parts-mail-id hdr))]
        [old-headers (mailbox-digest-mail-headers old-digest)]
        [new-headers (mailbox-digest-mail-headers new-digest)])
    ;; Keep all old headers, then add new ones with UIDs we haven't seen
    (let ([unique-new
           (for/list ([hdr new-headers]
                      #:when (not (set-member? seen-uids
                                               (main-mail-header-parts-mail-id hdr))))
             hdr)])
      (printf "Merging: ~a existing + ~a new = ~a total~n"
              (length old-headers)
              (length unique-new)
              (+ (length old-headers) (length unique-new)))
      (mailbox-digest
       (mailbox-digest-mail-address new-digest)
       (mailbox-digest-folder-name new-digest)
       (mailbox-digest-uid-validity new-digest)
       ;; Widen the range to cover both
       (cons (min (car (mailbox-digest-index-range old-digest))
                  (car (mailbox-digest-index-range new-digest)))
             (max (cdr (mailbox-digest-index-range old-digest))
                  (cdr (mailbox-digest-index-range new-digest))))
       (append old-headers unique-new)
       (mailbox-digest-timestamp new-digest)))))
