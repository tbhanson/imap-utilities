#lang racket

;; A structure to hold OAuth2 client credentials.
;; Stored alongside IMAP credentials in ~/.imap_secrets/

(struct oauth2-details
  (client-id client-secret redirect-uri)
  #:prefab
  )

(provide
 (contract-out
  [oauth2-details? (-> any/c boolean?)]
  [oauth2-details (-> string? string? string? oauth2-details?)]
  [oauth2-details-client-id (-> oauth2-details? string?)]
  [oauth2-details-client-secret (-> oauth2-details? string?)]
  [oauth2-details-redirect-uri (-> oauth2-details? string?)]
  ;; reading or writing
  [read-oauth2-details (-> port? oauth2-details?)]
  [read-one-oauth2-details-from-file-named (-> (or/c string? path?) oauth2-details?)]
  [oauth2-details-filename (-> string? path?)]
  [oauth2-details-filepath-from-dir (-> path? string? path?)]
  [default-oauth2-details-filepath (-> string? path?)]
  [write-oauth2-details-to-file (-> oauth2-details? path? void?)]
  ))

(define (read-oauth2-details a-port)
  (read a-port))
  
(define (read-one-oauth2-details-from-file-named filename)
  (call-with-input-file filename read-oauth2-details))

;; Filename to save these to or read from, keyed by email address
(define (oauth2-details-filename email-address)
  (let ([base ".oauth2_"])
    (build-path (format "~a~a" base email-address))))

;; Filepath relative to a given directory
(define (oauth2-details-filepath-from-dir dir email-address)
  (build-path dir (oauth2-details-filename email-address)))

;; Standard filepath under ~/.imap_secrets/
(define (default-oauth2-details-filepath email-address)
  (oauth2-details-filepath-from-dir
   (build-path (find-system-path 'home-dir) ".imap_secrets")
   email-address))

;; Write to file with owner-only permissions
(define (write-oauth2-details-to-file one-oauth2-details output-file-path)
  (call-with-output-file output-file-path
    (lambda (out) (write one-oauth2-details out))
    #:exists 'replace
    #:permissions #o600))
