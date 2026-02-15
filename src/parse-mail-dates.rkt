#lang racket

(require gregor)

(provide
 supported-mail-patterns
 parsable-as-datetime?
 supported-pattern-which-parses-date-time-string?
 possible-parse-date-time-string
 )

;; Date-time patterns found in real-world email headers.
;; Order matters: we try each in sequence and return the first match.
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

;; Some headers include a redundant parenthetical timezone name like "(CDT)".
;; Strip it, since it's ambiguous and the numeric offset is authoritative.
(define (remove-redundant-ambiguous-suffix date-time-string)
  (let ([possible-match (regexp-match-positions #px"[-+][0-9]{4}( [(][A-Z]+[)])$" date-time-string)])
    (if possible-match
        (substring date-time-string 0 (car (second possible-match)))
        date-time-string)))

;; Some headers use non-standard 4-letter day abbreviations like "Tues".
;; Strip the day-of-week prefix entirely (it's redundant with the date).
(define (remove-non-compliant-day-of-week-prefix date-time-string)
  (let ([possible-match (regexp-match-positions #px"^[A-Z][a-z]{3}, " date-time-string)])
    (if possible-match
        (substring date-time-string (cdr (first possible-match)))
        date-time-string)))

(define (sanitize-input-date-time-string date-time-string)
  (remove-non-compliant-day-of-week-prefix
   (remove-redundant-ambiguous-suffix date-time-string)))


(define (parsable-as-datetime? candidate-date-string date-string-pattern)
  (and (string? candidate-date-string)
       (string? date-string-pattern)
       (let ([tweaked (sanitize-input-date-time-string candidate-date-string)])
         (with-handlers ([exn:gregor:parse? (lambda (e) #f)])
           (datetime? (parse-datetime tweaked date-string-pattern))))))

  
(define (supported-pattern-which-parses-date-time-string? maybe-date-time-string)
  (let ([tweaked (sanitize-input-date-time-string maybe-date-time-string)])
    (for/first ([pattern supported-mail-patterns]
                #:when (parsable-as-datetime? tweaked pattern))
      pattern)))
  
(define (possible-parse-date-time-string maybe-date-time-string)
  (let ([tweaked (sanitize-input-date-time-string maybe-date-time-string)])
    (for/first ([pattern supported-mail-patterns]
                #:when (parsable-as-datetime? tweaked pattern))
      (parse-datetime tweaked pattern))))
