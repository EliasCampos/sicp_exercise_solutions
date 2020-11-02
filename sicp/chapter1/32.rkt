#lang sicp

(#%require rackunit)
(#%require racket/math)


(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))


(define (sum term a next b) (accumulate + 0 term a next b))

(define (product term a next b) (accumulate * 1 term a next b))


(define (square x) (* x x))

(define (ident x) x)
(define (one _) 1)
(define (cube x) (* x x x))
(define (vallis k) (/ (* k (+ k 2.0)) (square (+ k 1.0))))

(define-binary-check (check-in-tolerance actual expected)
  (< (abs (- actual expected)) 0.001))

(test-case
  "Should implement summing of numbers in range."
  (check-equal? (sum ident 1 inc 10) 55)
  (check-equal? (sum ident 1 inc 1) 1)
  (check-equal? (sum ident 1 inc 5) 15)
  (check-equal? (sum one 1 inc 10) 10)
  (check-equal? (sum cube 1 inc 10) 3025))


(test-case
  "Should implement product of numbers in range."
  (check-equal? (product ident 1 inc 3) 6)
  (check-equal? (product ident 1 inc 6) 720)
  (check-in-tolerance (product vallis 2 (lambda (k) (+ k 2)) 1000) (/ pi 4)))