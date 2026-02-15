#lang racket

;; A structure to hold IMAP account connection details.
;; Credentials are stored outside the codebase (e.g. ~/.imap_secrets/credentials)
;; in Racket's #prefab serialization format.

(struct imap-email-account-credentials
  (accountname hostname mailaddress password try-tls? xoauth2?)
  #:prefab
  )

(provide
 (contract-out
  ; automatic methods from struct
  [imap-email-account-credentials (-> string? string? string? string? boolean? boolean? imap-email-account-credentials?)]
  [imap-email-account-credentials? (-> any/c boolean?)]
  [imap-email-account-credentials-accountname (-> imap-email-account-credentials? string?)]
  [imap-email-account-credentials-hostname (-> imap-email-account-credentials? string?)]
  [imap-email-account-credentials-mailaddress (-> imap-email-account-credentials? string?)]
  [imap-email-account-credentials-password (-> imap-email-account-credentials? string?)]
  [imap-email-account-credentials-try-tls? (-> imap-email-account-credentials? boolean?)]
  [imap-email-account-credentials-xoauth2? (-> imap-email-account-credentials? boolean?)]
  ; reading one or several
  [read-one-email-account-credential (-> port? imap-email-account-credentials?)]
  [read-one-email-account-credentials-from-file-named (-> (or/c string? path?) imap-email-account-credentials?)]
  [read-email-account-credentials-hash-from-port (-> port? hash?)]
  [read-email-account-credentials-hash-from-file-named (-> (or/c string? path?) hash?)]
  ; convenience
  [default-secrets-dir (-> path?)]
  [default-credentials-filepath (-> path?)]
  )
 )


;; Keep personal account credentials in a standard, user-only-readable location.
;; Directory: ~/.imap_secrets/
;; File:      ~/.imap_secrets/credentials
(define (default-secrets-dir)
  (build-path (find-system-path 'home-dir) ".imap_secrets"))

(define (default-credentials-filepath)
  (build-path (default-secrets-dir) "credentials"))


;; We chose a serialization of the above such that all we need is read :)
;; cf. https://docs.racket-lang.org/guide/serialization.html#%28tech._serialization%29

(define (read-one-email-account-credential port)
  (read port))

(define (read-one-email-account-credentials-from-file-named filename)
  (call-with-input-file filename read-one-email-account-credential))

;; Read a list of credentials from a port, return a hash keyed by accountname.
(define (read-email-account-credentials-hash-from-port port)
  (let ((account-list (read port)))
    (for/fold ([result (make-immutable-hash)])
              ([account account-list])
      (let ([key (imap-email-account-credentials-accountname account)])
        (values (hash-set result key account))))))

(define (read-email-account-credentials-hash-from-file-named filename)
  (call-with-input-file filename read-email-account-credentials-hash-from-port))
