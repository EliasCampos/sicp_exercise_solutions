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
  (define (dispatch m)
    (cond [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request: MAKE-ACCOUNT" m)]))
  (password-protection dispatch password))

(define (password-protection account password)
  (define (password-protected-call passkey f)
    (lambda (x)
      (if (not (eq? password passkey))
        "Incorrect password"
        (f x))))

  (define (make-joint-acc new-password)
    (password-protection account new-password))

  (define (dispatch passkey m)
    (let ([op (cond [(eq? m 'make-joint) make-joint-acc]
                    [else (account m)])])
      (password-protected-call passkey op)))
  dispatch)
        
(define (make-joint account old-password new-password)
  ((account old-password 'make-joint) new-password))

(define acc (make-account 100 'secret-password))

(check-eq? ((acc 'secret-password 'withdraw) 40) 60)
(check-eq? ((acc 'some-other-password 'deposit) 50) "Incorrect password")
(check-eq? ((acc 'secret-password 'deposit) 50) 110)
(check-eq? ((acc 'no-way 'withdraw) 20) "Incorrect password")
(check-eq? ((acc 'secret-password 'withdraw) 20) 90)

(define joint-acc (make-joint acc 'secret-password 'new-secret-password))

(check-eq? ((joint-acc 'new-secret-password 'withdraw) 25) 65)
(check-eq? ((joint-acc 'secret-password 'deposit) 100) "Incorrect password")
(check-eq? ((joint-acc 'new-secret-password 'deposit) 100) 165)
(check-eq? ((acc 'secret-password 'withdraw) 25) 140)
