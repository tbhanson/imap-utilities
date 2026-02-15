#lang racket

(require openssl
         net/imap
         "imap-email-account-credentials.rkt")

(provide
 securely-connect-to-imap-account
 )

;; Connect to an IMAP account over SSL using password authentication.
;; Returns the imap-connection object (caller is responsible for disconnecting).
(define (securely-connect-to-imap-account one-imap-email-account-credential mail-folder)
  (let ((port-no 993)
        (hostname (imap-email-account-credentials-hostname one-imap-email-account-credential))
        (mailaddress (imap-email-account-credentials-mailaddress one-imap-email-account-credential))
        (password (imap-email-account-credentials-password one-imap-email-account-credential)))

    (let-values
        ([(imap-connection messages# nu)
          (let-values ([(in out) (ssl-connect hostname port-no)])
            (imap-connect* in out mailaddress password mail-folder))])
      (begin
        (printf "connected to ~a at ~a; messages: ~a; new: ~a~n"
                mailaddress hostname messages# nu)
        imap-connection))))
