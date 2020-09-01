#lang sicp

(#%require rackunit)
(#%require racket/math)

(define (product term a next b)
  (define (product-iter k result)   
    (if (> k b)
        result
        (product-iter (next k) (* (term k) result))))
  (product-iter a 1))

(define (square x) (* x x))

(define (ident x) x)
(define (vallis k) (/ (* k (+ k 2.0)) (square (+ k 1.0))))


(define-binary-check (check-in-tolerance actual expected)
  (< (abs (- actual expected)) 0.001))

(test-case
  "Should implement product of numbers in range."
  (check-equal? (product ident 1 inc 3) 6)
  (check-equal? (product ident 1 inc 6) 720)
  (check-in-tolerance (product vallis 2 (lambda (k) (+ k 2)) 1000) (/ pi 4)))
