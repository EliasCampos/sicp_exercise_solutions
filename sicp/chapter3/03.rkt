#lang sicp

(#%require rackunit)

(define (make-account balance password)  
  (define (withdraw amount)
    (cond [(< balance amount) "Insufficient funds"]
          [else (set! balance (- balance amount))
                balance]))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch passkey m)
    (define (password-protected f)
      (lambda (amount)
        (if (not (eq? password passkey))
            "Incorrect password"
            (f amount))))
    (cond [(eq? m 'withdraw) (password-protected withdraw)]
          [(eq? m 'deposit) (password-protected deposit)]
          [else (error "Unknown request: MAKE-ACCOUNT" m)]))
  dispatch)


(define acc (make-account 100 'secret-password))

(check-eq? ((acc 'secret-password 'withdraw) 40) 60)
(check-equal? ((acc 'some-other-password 'deposit) 50) "Incorrect password")
(check-equal? ((acc 'secret-password 'deposit) 50) 110)
(check-eq? ((acc 'no-way 'withdraw) 20) "Incorrect password")
(check-eq? ((acc 'secret-password 'withdraw) 20) 90)