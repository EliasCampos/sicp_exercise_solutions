#lang sicp

(#%require rackunit)

(define (double f)
  (lambda (x) (f (f x))))



(test-case
  "Should implement procedure to apply function twice."
  (check-equal? ((double inc) 0) 2)
  (check-equal? ((double inc) 5) 7)
  (check-equal? ((double (double inc)) 0) 4)
  (check-equal? (((double double) inc) 5) 9)
  (check-equal? (((double (double double)) inc) 5) 21))