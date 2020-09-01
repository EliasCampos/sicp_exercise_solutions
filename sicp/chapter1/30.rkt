#lang sicp

(#%require rackunit)

(define (sum term a next b)
  (define (sum-iter k result)
    (if (> k b)
        result
        (sum-iter (next k) (+ (term k) result))))
  (sum-iter a 0))

(define (ident x) x)
(define (one _) 1)
(define (cube x) (* x x x))

(test-case
  "Should implement summing of numbers in range."
  (check-equal? (sum ident 1 inc 10) 55)
  (check-equal? (sum ident 1 inc 1) 1)
  (check-equal? (sum ident 1 inc 5) 15)
  (check-equal? (sum one 1 inc 10) 10)
  (check-equal? (sum cube 1 inc 10) 3025))
