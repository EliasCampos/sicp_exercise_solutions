#lang sicp


(#%require rackunit)

(define (square x) (* x x))
(define (divides? divisor number) (= (remainder number divisor) 0))
(define (smallest-divisor n)
  (define (next test-divisor)
    (if (= test-divisor 2) 3 (+ test-divisor 2)))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (next test-divisor)))))

  (find-divisor 2))


(test-case
  "Shoud find smallest divisor greater than 1."
  (check-equal? (smallest-divisor 2) 2)
  (check-equal? (smallest-divisor 3) 3)
  (check-equal? (smallest-divisor 8) 2)
  (check-equal? (smallest-divisor 25) 5)
  (check-equal? (smallest-divisor 11) 11))