#lang sicp

(define (average a b) (/ (+ a b) 2))

(define (good-enough? guess old-guess)
  (< (/ (abs (- guess old-guess)) guess) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter old-guess guess x)
  (if (good-enough? guess old-guess)
      guess
      (sqrt-iter guess (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 (improve 1.0 x) x))

(sqrt 0.0000009)
(sqrt 340000580)