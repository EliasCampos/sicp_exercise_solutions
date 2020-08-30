#lang sicp

(#%require rackunit)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(test-case
  "Should implement composition of two functions."
  (check-equal? ((compose square inc) 6) 49)
  (check-equal? ((compose square inc) 9) 100)
  (check-equal? ((compose inc square) 2) 5))
