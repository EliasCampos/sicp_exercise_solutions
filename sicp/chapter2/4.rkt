#lang sicp

(#%require rackunit)

(define (my-cons x y)
  (lambda (m) (m x y)))
(define (my-car z)
  (z (lambda (p _) p)))
(define (my-cdr z)
  (z (lambda (_ q) q)))


(test-case
  "Should return left element of the pair."
  (check-equal? (my-car (my-cons 2 3)) 2)
  (check-equal? (my-car (my-cons -1 0)) -1))
(test-case
  "Should return right element of the pair."
  (check-equal? (my-cdr (my-cons 2 3)) 3)
  (check-equal? (my-cdr (my-cons -1 0)) 0))