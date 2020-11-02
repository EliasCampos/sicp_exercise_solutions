#lang sicp

(#%require rackunit)
(#%require math)

(define (iterative-improve good-enough? improve)
  (lambda (init)
    (define (iterative-improve-iter result)
      (if (good-enough? result)
          result
          (iterative-improve-iter (improve result))))
    (iterative-improve-iter init)))


(define PRECISION 0.0001)
(define (square x)
  (define (good-enough? val) (<= (abs (- (* val val) x)) PRECISION))
  (define (improve val) (/ (+ val (/ x val)) 2))

  ((iterative-improve good-enough? improve) 1))

(define (fixed-point f first-guess)
  (define (close-enough? val) (< (abs (- val (f val))) PRECISION))

  ((iterative-improve close-enough? f) first-guess))
  

(define-binary-check (check-in-tolerance actual expected)
  (< (abs (- actual expected)) PRECISION))

(test-case
  "Should search square root using iterative improve as base."
  (check-in-tolerance (square 1.0) 1.0)
  (check-in-tolerance (square 9.0) 3.0)
  (check-in-tolerance (square 2.0) 1.4142))


(test-case
  "Should calculate fixed point using iterative improve as base."
  (check-in-tolerance (fixed-point cos 1) 0.7391)
  (check-in-tolerance (fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0) 1.2587))
