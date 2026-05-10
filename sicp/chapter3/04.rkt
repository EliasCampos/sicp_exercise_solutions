#lang sicp

(#%require rackunit)


(define (call-the-cops) "Got ya! The police are coming!")

(define (make-account balance password)
  (define password-attempts 7)
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
        (cond [(not (eq? password passkey))
               (set! password-attempts (dec password-attempts))
               (if (<= password-attempts 0)
                   (call-the-cops)
                   "Incorrect password")]
              [else
               (set! password-attempts 7)
               (f amount)])))
    (cond [(eq? m 'withdraw) (password-protected withdraw)]
          [(eq? m 'deposit) (password-protected deposit)]
          [else (error "Unknown request: MAKE-ACCOUNT" m)]))
  dispatch)


(define acc (make-account 100 'secret-password))

(check-eq? ((acc 'secret-password 'withdraw) 40) 60)
(check-eq? ((acc 'some-other-password 'deposit) 50) "Incorrect password")
(check-eq? ((acc 'secret-password 'deposit) 50) 110)
(check-eq? ((acc 'no-way 'withdraw) 20) "Incorrect password")
(check-eq? ((acc 'secret-password 'withdraw) 20) 90)

(check-eq? ((acc 'no-way 'withdraw) 1) "Incorrect password")
(check-eq? ((acc 'some-other-password 'deposit) 2) "Incorrect password")
(check-eq? ((acc 'no-way 'deposit) 3) "Incorrect password")
(check-eq? ((acc 'some-other-password 'deposit) 4) "Incorrect password")
(check-eq? ((acc 'no-way 'withdraw) 5) "Incorrect password")
(check-eq? ((acc 'some-other-password 'withdraw) 6) "Incorrect password")
(check-eq? ((acc 'no-way 'withdraw) 7) "Got ya! The police are coming!")
(check-eq? ((acc 'no-way 'withdraw) 8) "Got ya! The police are coming!")
(check-eq? ((acc 'secret-password 'withdraw) 5) 85)