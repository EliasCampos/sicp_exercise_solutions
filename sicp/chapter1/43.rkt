#lang sicp

(#%require rackunit)

(define (repeated f n)
  (lambda (x)
    (define (repeated-iter k result)
      (if (<= k 0)
          result
          (repeated-iter (- k 1) (f result))))
  
     (repeated-iter n x)))

(define (square x) (* x x))
(test-case
  "Should return a function, that applies given n times."
  (check-equal? ((repeated inc 10) 1)  11)
  (check-equal? ((repeated inc 3) 7)  10)
  (check-equal? ((repeated square 3) 2) 256)
  (check-equal? ((repeated square 2) 5) 625))
