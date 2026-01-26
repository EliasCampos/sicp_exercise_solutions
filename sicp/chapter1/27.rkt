#lang sicp

(#%require rackunit)

(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (ferma-positive? n)
  (define (ferma-positive-iter a)
    (cond ((not (< a n)) true)
          ((= (expmod a n n) a) (ferma-positive-iter (+ a 1)))
          (else false)))
  (ferma-positive-iter 2))


(test-case
  "The procedure has to return true for prime numbers."
  (check-true (ferma-positive? 2))
  (check-true (ferma-positive? 3))
  (check-true (ferma-positive? 5))
  (check-true (ferma-positive? 7))
  (check-true (ferma-positive? 11)))

(test-case
  "The procedure has to return false for non-prime numbers except for Carmichael numbers."
  (check-false (ferma-positive? 4))
  (check-false (ferma-positive? 6))
  (check-false (ferma-positive? 10))
  (check-false (ferma-positive? 12))
  (check-false (ferma-positive? 15)))

(test-case
  "The procedure has to return true for Carmichael numbers."
  (check-true (ferma-positive? 561))
  (check-true (ferma-positive? 1105))
  (check-true (ferma-positive? 1729))
  (check-true (ferma-positive? 2465))
  (check-true (ferma-positive? 2821))
  (check-true (ferma-positive? 6601)))
