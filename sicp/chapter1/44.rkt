#lang sicp

(#%require rackunit)

(define dx 1)

(define (smooth f)
  (define (avg x y z) (/ (+ x y z) 3))
  (lambda (x) (avg (f (- x dx)) (f x) (f (+ x dx)))))


(define (square x) (* x x))

(test-case
  "Should return smoothed versin of a function."
  (check-equal? ((smooth square) 1.0)  (/ 5.0 3.0))
  (check-equal? ((smooth square) -2.0) (/ 14.0 3.0)))
