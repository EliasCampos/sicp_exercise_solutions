#lang sicp

(#%require rackunit)

(define (square x) (* x x))
(define (square-check x n m)
  (cond ((and (> x 1) (< x n) (= (remainder (square x) m) 1))
         0)
        (else (square x))))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square-check (expmod base (/ exp 2) m) exp m)
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ (random (- n 1)) 1)))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(test-case
  "The Miller-Rabit test has to return true for prime numbers."
  (check-true (fast-prime? 2 100))
  (check-true (fast-prime? 3 100))
  (check-true (fast-prime? 5 100))
  (check-true (fast-prime? 7 100))
  (check-true (fast-prime? 101 100))
  (check-true (fast-prime? 103 100))
  (check-true (fast-prime? 199 100))
  (check-true (fast-prime? 233 100))
  (check-true (fast-prime? 1009 100))
  (check-true (fast-prime? 7919 100)))

(test-case
  "The Miller-Rabit test has to return false for numbers which are not prime."
  (check-false (fast-prime? 4 100))
  (check-false (fast-prime? 6 100))
  (check-false (fast-prime? 10 100))
  (check-false (fast-prime? 12 100))
  (check-false (fast-prime? 15 100))
  (check-false (fast-prime? 104 100))
  (check-false (fast-prime? 1011 100))
  (check-false (fast-prime? 1915 100))
  (check-false (fast-prime? 5703 100)))