#lang racket

;; Derive "people I've written to" from the To/Cc/Bcc fields of all
;; messages in Sent folders across all accounts.
;;
;; Usage:
;;   racket derive-contacts.rkt              ; show summary
;;   racket derive-contacts.rkt --write      ; write derived-contacts.txt
;;
;; Produces ~/.imap_secrets/derived-contacts.txt — a list of email
;; addresses you've ever sent mail to. This is used in addition to
;; known-contacts.txt to protect senders from automatic purging.
;;
;; Re-run this whenever you've fetched new sent mail, or before any
;; automatic-purge workflow.

(require
  "src/mailbox-digest.rkt"
  "src/main-mail-header-parts.rkt"
  net/head
  gregor)

;; ---- digest loading ----

(define (default-digest-dir)
  (build-path (find-system-path 'home-dir) ".imap_secrets" "digests"))

(define (default-derived-contacts-filepath)
  (build-path (find-system-path 'home-dir) ".imap_secrets"
              "derived-contacts.txt"))

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

;; A digest is for a Sent folder if its folder name matches common
;; Sent-folder patterns across IMAP servers and Gmail localizations.
(define sent-folder-rx
  #rx"(?i:sent|gesendet|envoy|inviati|enviados|verzonden)")

(define (sent-digests digests)
  (filter (lambda (mbd)
            (regexp-match? sent-folder-rx (mailbox-digest-folder-name mbd)))
          digests))

;; ---- address extraction ----

;; Extract bare email address from a string like:
;;   "Foo Bar <foo@example.com>"  →  "foo@example.com"
;;   "foo@example.com"             →  "foo@example.com"
;; Returns lowercased address or #f.
(define angle-rx #rx"<([^>@]+@[^>]+)>")
(define bare-rx #rx"([A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,})")

(define (extract-email-from-fragment frag)
  (let ([m (or (regexp-match angle-rx frag)
               (regexp-match bare-rx frag))])
    (and m (string-downcase (second m)))))

;; A single header field (To/Cc/Bcc) can contain multiple addresses,
;; comma-separated, possibly with quoted display names containing commas.
;; We use a simple approach: split by comma but require @ in each piece.
(define (extract-emails-from-header-field s)
  (if (or (not s) (string=? s ""))
      '()
      (let ([pieces (regexp-split #rx"," s)])
        (filter values
                (map extract-email-from-fragment pieces)))))

;; Collect all addresses I've written to from a single sent-folder digest.
(define (addresses-from-sent-digest mbd)
  (let ([result (mutable-set)])
    (for ([hdr (mailbox-digest-mail-headers mbd)])
      (unless (main-mail-header-parts-deleted? hdr)
        (for ([field (list (main-mail-header-parts-to hdr)
                           (main-mail-header-parts-cc hdr)
                           (main-mail-header-parts-bcc hdr))])
          (for ([addr (extract-emails-from-header-field field)])
            (set-add! result addr)))))
    result))

;; ---- main ----

(define (parse-args args)
  (let ([arg-list (vector->list args)]
        [write? #f])
    (for ([a arg-list])
      (when (string=? a "--write") (set! write? #t)))
    write?))

(define (main)
  (let ([write? (parse-args (current-command-line-arguments))]
        [digests (load-all-latest-digests)])
    (let ([sent (sent-digests digests)])
      (when (null? sent)
        (printf "No Sent-folder digests found.~n")
        (printf "Make sure you've fetched at least one sent folder per account.~n")
        (exit 0))

      (printf "~nScanning ~a sent-folder digest(s):~n" (length sent))
      (let ([all-addresses (mutable-set)]
            [per-account (make-hash)])

        (for ([mbd sent])
          (let* ([email (mailbox-digest-mail-address mbd)]
                 [folder (mailbox-digest-folder-name mbd)]
                 [addrs (addresses-from-sent-digest mbd)])
            (printf "  ~a / ~a: ~a unique addresses (from ~a sent messages)~n"
                    email folder (set-count addrs)
                    (length (mailbox-digest-mail-headers mbd)))
            (for ([a (in-set addrs)])
              (set-add! all-addresses a))
            (hash-set! per-account (cons email folder) addrs)))

        (printf "~nTotal unique addresses across all sent folders: ~a~n"
                (set-count all-addresses))

        (when write?
          (let ([path (default-derived-contacts-filepath)])
            (call-with-output-file path
              (lambda (out)
                (fprintf out "# Auto-derived from Sent folders by derive-contacts.rkt~n")
                (fprintf out "# Generated ~a~n" (current-seconds))
                (fprintf out "# Do not edit by hand — re-run derive-contacts.rkt~n~n")
                (for ([a (sort (set->list all-addresses) string<?)])
                  (fprintf out "~a~n" a)))
              #:exists 'replace)
            (printf "Wrote ~a addresses to ~a~n"
                    (set-count all-addresses) path)))

        (unless write?
          (printf "~n(Use --write to save to ~a)~n"
                  (default-derived-contacts-filepath)))))))

(main)
