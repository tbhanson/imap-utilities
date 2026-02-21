#lang racket

(require net/head
         gregor)

;; The subset of mail header fields we care about for analysis.
;; parsed-year and parsed-epoch are computed at fetch time from date-string,
;; enabling fast date filtering without re-parsing.
(struct main-mail-header-parts
  (mail-id date-string from to cc bcc subj flags parsed-year parsed-epoch)
  #:prefab
  )

(provide
 (contract-out
  ;; struct automatics
  [main-mail-header-parts (-> integer? string? string? string? string? string? string? list?
                              (or/c integer? #f) (or/c integer? #f)
                              main-mail-header-parts?)]
  [main-mail-header-parts? (-> any/c boolean?)]
  [main-mail-header-parts-mail-id (-> main-mail-header-parts? integer?)]
  [main-mail-header-parts-date-string (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-from (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-to (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-cc (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-bcc (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-subj (-> main-mail-header-parts? string?)]
  [main-mail-header-parts-flags (-> main-mail-header-parts? list?)]
  [main-mail-header-parts-parsed-year (-> main-mail-header-parts? (or/c integer? #f))]
  [main-mail-header-parts-parsed-epoch (-> main-mail-header-parts? (or/c integer? #f))]

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

;; Date parsing helpers — computed once at fetch time.
;; We try multiple date formats and extract year + epoch seconds.

(define supported-mail-patterns
  (list
   "yyyy-MM-dd'T'HH:mm:ssxxx"
   "dd MMM yyyy HH:mm:ss"
   "dd MMM yyyy HH:mm:ss xxxx"
   "d MMM yyyy HH:mm:ss xxxx"
   "EEE, dd MMM yyyy HH:mm:ss xxxx"
   "EEE, dd MM yyyy HH:mm:ss"
   "EEE, d MMM yyyy HH:mm:ss xxxx"
   ))

(define (sanitize-date-string s)
  ;; Strip redundant parenthetical timezone like "(CDT)"
  (let ([s1 (let ([m (regexp-match-positions #px"[-+][0-9]{4}( [(][A-Z]+[)])$" s)])
              (if m (substring s 0 (car (second m))) s))])
    ;; Strip non-standard 4-letter day abbreviations
    (let ([m (regexp-match-positions #px"^[A-Z][a-z]{3}, " s1)])
      (if m (substring s1 (cdr (first m))) s1))))

(define (try-parse-datetime date-string)
  (let ([tweaked (sanitize-date-string date-string)])
    (for/first ([pattern supported-mail-patterns]
                #:when (with-handlers ([exn:fail? (lambda (e) #f)])
                         (datetime? (parse-datetime tweaked pattern))))
      (parse-datetime tweaked pattern))))

(define (datetime->epoch dt)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (->posix dt)))

;; Convert a raw IMAP message (list of uid, header-bytes, flags)
;; into our main-mail-header-parts struct.
(define (mail-header->main-mail-header-parts msg)
  (define (field-contents field header)
    (with-handlers
        ([exn:fail? (lambda (e) "")]) ; missing field -> empty string
      (bytes->string/utf-8 (extract-field field header))))
  
  (let* ([uid (first msg)]
         [header (second msg)]
         [flags (third msg)]
         [date-str (field-contents #"date" header)]
         [parsed-dt (try-parse-datetime date-str)]
         [year (and parsed-dt (->year parsed-dt))]
         [epoch (and parsed-dt (datetime->epoch parsed-dt))])
    (main-mail-header-parts
     uid
     date-str
     (field-contents #"from" header)
     (field-contents #"to" header)
     (field-contents #"cc" header)
     (field-contents #"bcc" header)
     (field-contents #"subject" header)
     flags
     year
     epoch)))
