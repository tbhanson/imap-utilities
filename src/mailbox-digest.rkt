#lang racket

(require
  "imap-email-account-credentials.rkt"
  "connect-to-imap-account.rkt"
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
 ))


;; Fetch headers from an IMAP server and build a local digest.
(define (get-mailbox-digest mail-account-credential folder-name item-index-range)
  (let ([imap-conn
         (securely-connect-to-imap-account mail-account-credential folder-name)]
        [now-timestamp (now)])
    (let ([msg-count (imap-messages imap-conn)]
          [uid-validity (imap-uidvalidity imap-conn)]
          [lo-index (car item-index-range)])
      (let ([hi-index (min msg-count (cdr item-index-range))])
        (printf "~a messages in ~a; examining ~a to ~a~n"
                msg-count folder-name lo-index hi-index)
        (let ([range-of-message-headers
               (map
                mail-header->main-mail-header-parts
                (imap-get-messages
                 imap-conn
                 (stream->list (in-range lo-index (+ hi-index 1)))
                 main-mail-header-part-imap-symbols))])
          (imap-disconnect imap-conn)
          (mailbox-digest
           (imap-email-account-credentials-mailaddress mail-account-credential)
           folder-name
           uid-validity
           item-index-range
           range-of-message-headers
           now-timestamp))))))

;; Save serialized digest to a file in the given directory; return the full path.
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
