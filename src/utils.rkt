#lang racket

;; Shared utilities for the imap-utilities project.
;;
;; This module collects helper functions that were duplicated across
;; multiple tools. It contains no IO-driven logic (no IMAP, no OAuth2),
;; only pure helpers and path/file conventions.

(require racket/format
         racket/contract
         racket/file
         racket/string
         racket/path
         racket/set
         net/head
         gregor
         "mailbox-digest.rkt"
         "main-mail-header-parts.rkt"
         "known-contacts.rkt")

;; ---- standard paths ----
;;
;; Some path helpers already live in their respective modules and
;; are not (re-)exported here:
;;   - default-credentials-filepath : imap-email-account-credentials.rkt
;;   - default-secrets-dir          : imap-email-account-credentials.rkt
;;   - default-known-contacts-filepath : known-contacts.rkt
;;   - default-digest-dir           : mailbox-digest.rkt
;;
;; This module only adds path helpers that were NOT already provided.
;; We use a local secrets-dir helper to avoid the import dependency
;; while still anchoring our paths in the same .imap_secrets directory.

(define (-secrets-dir)
  (build-path (find-system-path 'home-dir) ".imap_secrets"))

(define (default-derived-contacts-filepath)
  (build-path (-secrets-dir) "derived-contacts.txt"))

(define (default-status-visits-filepath)
  (build-path (-secrets-dir) "status-visits.txt"))

;; ---- size formatting ----

;; Format a byte count for display. Handles #f and 0 by returning "-".
;; Picks the largest unit that yields a value >= 1.
;;
;; Examples:
;;   (format-size #f)              => "-"
;;   (format-size 0)               => "-"
;;   (format-size 512)             => "512 B"
;;   (format-size 2048)            => "2 KB"
;;   (format-size 5242880)         => "5.0 MB"
;;   (format-size 2147483648)      => "2.00 GB"
(define (format-size bytes)
  (cond
    [(or (not bytes) (= bytes 0)) "-"]
    [(>= bytes (* 1024 1024 1024))
     (format "~a GB" (~r (/ bytes 1024.0 1024.0 1024.0) #:precision '(= 2)))]
    [(>= bytes (* 1024 1024))
     (format "~a MB" (~r (/ bytes 1024.0 1024.0) #:precision '(= 1)))]
    [(>= bytes 1024)
     (format "~a KB" (inexact->exact (round (/ bytes 1024.0))))]
    [else
     (format "~a B" bytes)]))

;; Format a kilobyte count for display. The IMAP QUOTA RFC reports
;; storage in kilobytes, so this is a convenience wrapper.
(define (format-size-kb kb)
  (format-size (and kb (* kb 1024))))

;; ---- truncation ----

;; Truncate a string to at most max-len characters, appending "..."
;; if truncation occurred. The "..." counts toward max-len.
(define (truncate-string s max-len)
  (if (<= (string-length s) max-len)
      s
      (string-append (substring s 0 (max 0 (- max-len 3))) "...")))

;; ---- I/O ----

;; Configure stdout to handle broken pipe gracefully (e.g. when piping to head).
;; Switch to line buffering and silently swallow EPIPE / "broken pipe" errors.
;; Call once near the top of a script's main module.
(define (handle-broken-pipe)
  (file-stream-buffer-mode (current-output-port) 'line)
  (uncaught-exception-handler
   (let ([prev (uncaught-exception-handler)])
     (lambda (e)
       (if (and (exn:fail? e)
                (regexp-match? #rx"[Bb]roken pipe" (exn-message e)))
           (exit 0)
           (prev e))))))

;; ---- digest loading ----

;; Load the latest digest for each (account, folder) pair from the
;; given digest directory (or default if not specified).
;;
;; Returns a list of mailbox-digest? structs.
;;
;; If the same (account, folder) pair has multiple digest files
;; (because multiple snapshots exist over time), only the one with
;; the most recent timestamp is returned.
(define (load-all-latest-digests
         [dir (build-path (find-system-path 'home-dir)
                          ".imap_secrets" "digests")])
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
      '()))

;; ---- folder filters ----

;; Regex for INBOX folder name (case-insensitive).
(define inbox-folder-rx #rx"(?i:^inbox$)")

;; Regex for Sent folder names across IMAP servers and Gmail localizations.
;; Matches: Sent, Gesendet (de), Envoy* (fr), Inviati (it), Enviados (es), Verzonden (nl)
(define sent-folder-rx
  #rx"(?i:sent|gesendet|envoy|inviati|enviados|verzonden)")

;; Filter digests to INBOX folders only (strict: folder name literally INBOX).
(define (inbox-digests digests)
  (filter (lambda (mbd)
            (regexp-match? inbox-folder-rx (mailbox-digest-folder-name mbd)))
          digests))

;; Filter digests to Sent folders only.
(define (sent-digests digests)
  (filter (lambda (mbd)
            (regexp-match? sent-folder-rx (mailbox-digest-folder-name mbd)))
          digests))

;; Filter digests to non-Sent folders (permissive: everything except sent).
;; Useful for purging mail that arrived in any received folder, not just
;; the strict INBOX.
(define (non-sent-digests digests)
  (filter (lambda (mbd)
            (not (regexp-match? sent-folder-rx (mailbox-digest-folder-name mbd))))
          digests))

;; ---- address extraction ----

;; Extract the bare email address from a From-header string.
;; Returns "" if no address can be parsed (or if the parser hands us
;; something that doesn't look like an email — e.g. just a display name).
;;
;; Examples:
;;   (extract-from-addr "Foo Bar <foo@example.com>")  => "foo@example.com"
;;   (extract-from-addr "foo@example.com")             => "foo@example.com"
;;   (extract-from-addr "")                            => ""
;;   (extract-from-addr "no email here")               => ""
(define (extract-from-addr from-str)
  (with-handlers ([exn:fail? (lambda (e) "")])
    (let ([addrs (extract-addresses from-str 'address)])
      (if (null? addrs)
          ""
          (let ([first-addr (string-downcase (first addrs))])
            (if (regexp-match? #rx"@" first-addr)
                first-addr
                ""))))))

;; ---- combined known + derived contacts ----

;; Load both known-contacts and derived-contacts as a single set
;; of email addresses (lowercased).
;;
;; - known-contacts: hand-curated, in ~/.imap_secrets/known-contacts
;; - derived-contacts: auto-derived from sent folders, in
;;   ~/.imap_secrets/derived-contacts.txt (generated by derive-contacts.rkt)
(define (load-all-known-contacts)
  (let ([known (with-handlers ([exn:fail? (lambda (e) (set))])
                 (load-known-contacts (default-known-contacts-filepath)))]
        [derived (with-handlers ([exn:fail? (lambda (e) (set))])
                   (let ([path (default-derived-contacts-filepath)])
                     (if (file-exists? path)
                         (let ([result (mutable-set)])
                           (for ([line (file->lines path)])
                             (let ([trimmed (string-trim line)])
                               (unless (or (string=? trimmed "")
                                           (regexp-match? #rx"^#" trimmed))
                                 (set-add! result (string-downcase trimmed)))))
                           result)
                         (set))))])
    (set-union known derived)))

;; ---- exports ----

(provide
 (contract-out
  ;; paths
  [default-derived-contacts-filepath (-> path?)]
  [default-status-visits-filepath (-> path?)]

  ;; formatting
  [format-size (-> (or/c #f exact-nonnegative-integer?) string?)]
  [format-size-kb (-> (or/c #f exact-nonnegative-integer?) string?)]
  [truncate-string (-> string? exact-nonnegative-integer? string?)]

  ;; I/O
  [handle-broken-pipe (-> any)]

  ;; digest loading
  [load-all-latest-digests (->* () (path?) list?)]
  [inbox-digests (-> list? list?)]
  [sent-digests (-> list? list?)]
  [non-sent-digests (-> list? list?)]

  ;; address extraction
  [extract-from-addr (-> string? string?)]

  ;; contacts
  [load-all-known-contacts (-> set?)]))
