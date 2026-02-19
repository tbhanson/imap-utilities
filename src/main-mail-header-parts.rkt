#lang racket

(require net/head)

;; The subset of mail header fields we care about for analysis.
(struct main-mail-header-parts
  (mail-id date-string from to cc bcc subj flags)
  #:prefab
  )

(provide
 (contract-out
  ;; struct automatics
  [main-mail-header-parts (-> integer? string? string? string? string? string? string? list? main-mail-header-parts?)]
  [main-mail-header-parts? (-> any/c boolean?)]
  [main-mail-header-parts-mail-id (-> main-mail-header-parts? integer?)]
  [main-mail-header-parts-date-string (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-from (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-to (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-cc (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-bcc (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-subj (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-flags (-> main-mail-header-parts? list?)]

  ;; converter: raw IMAP message -> our struct
  [mail-header->main-mail-header-parts (-> (and/c pair? list?) main-mail-header-parts?)]
  )
 
 ;; constants
 main-mail-header-part-labels
 main-mail-header-part-imap-symbols
 )

(define main-mail-header-part-labels
  (list #"date" #"from" #"to" #"cc" #"bcc" #"subject"))

(define main-mail-header-part-imap-symbols
  '(uid header flags))

;; Convert a raw IMAP message (list of uid, header-bytes, flags)
;; into our main-mail-header-parts struct.
(define (mail-header->main-mail-header-parts msg)
  (define (field-contents field header)
    (with-handlers
        ([exn:fail? (lambda (e) "")]) ; missing field -> empty string
      (bytes->string/utf-8 (extract-field field header))))
  
  (let ([uid (first msg)]
        [header (second msg)]
        [flags (third msg)])
    (main-mail-header-parts
     uid
     (field-contents #"date" header)
     (field-contents #"from" header)
     (field-contents #"to" header)
     (field-contents #"cc" header)
     (field-contents #"bcc" header)
     (field-contents #"subject" header)
     flags)))
