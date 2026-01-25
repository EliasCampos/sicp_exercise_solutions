#lang sicp

(#%require rackunit)

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

(check-equal? (smallest-divisor 2) 2)
(check-equal? (smallest-divisor 10) 2)
(check-equal? (smallest-divisor 9) 3)
(check-equal? (smallest-divisor 49) 7)
(check-equal? (smallest-divisor 47) 47)
(check-equal? (smallest-divisor 121) 11)

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)