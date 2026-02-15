#lang racket

(provide
 parse-to-address-statistics
 )

(require net/head)

;; Given an enumerable of to/cc/bcc address strings and the user's own address,
;; partition them into those that include the user and those that don't.
;; Returns a dispatch function supporting 'including-me and 'not-including-me.
(define (parse-to-address-statistics enumerable-of-email-address-strings my-address)
  (define (address-matches addr1 addr2)
    (string-ci=? addr1 addr2))
  
  (define (includes-me? address-chain)
    (let ([moi (first (extract-addresses my-address 'address))])
      (for/first ([next-address (extract-addresses address-chain 'address)]
                  #:when (address-matches moi next-address))
        #t)))

  (let-values
      ([(including-me not-including-me)
        (for/fold ([with-me '()]
                   [without-me '()])
                  ([next-item enumerable-of-email-address-strings])
          (if (includes-me? next-item)
              (values (cons next-item with-me) without-me)
              (values with-me (cons next-item without-me))))])

    (define (dispatch msg)
      (cond
        [(eq? msg 'including-me)    including-me]
        [(eq? msg 'not-including-me) not-including-me]
        [else (error 'parse-to-address-statistics "unknown query: ~a" msg)]))
    dispatch))
