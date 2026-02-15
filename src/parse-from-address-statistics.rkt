#lang racket

(provide
 parse-from-address-statistics
 )

(require net/head)

;; Given an enumerable of email address strings (as they appear in From: headers),
;; return a hash: normalized-address -> count
(define (parse-from-address-statistics enumerable-of-email-address-strings)
  (let ([just-addresses
         (for/fold ([addrs '()])
                   ([next-item enumerable-of-email-address-strings])
           (values (append addrs (extract-addresses next-item 'address))))])
    (for/fold ([result (hash)])
              ([addr just-addresses])
      (values (hash-update result addr add1 0)))))
