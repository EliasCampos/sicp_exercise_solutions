#lang sicp

(#%require rackunit)

(define (mul a b)
  (define (double x) (* 2 x))
  (define (halve x) (/ x 2))
  
  (cond ((= b 0) 0)
        ((even? b) (double (mul a (halve b))))
        (else (+ a (mul a (- b 1))))))

(check-equal? (mul 0 0) 0)
(check-equal? (mul 1 0) 0)
(check-equal? (mul 42 0) 0)
(check-equal? (mul 1 5) 5)
(check-equal? (mul 3 4) 12)
(check-equal? (mul 5 9) 45)
(check-equal? (mul 11 11) 121)