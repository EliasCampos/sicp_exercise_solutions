#lang sicp

(#%require rackunit)

(define (make-accumulator value)
  (lambda (x)
    (begin (set! value (+ value x))
           value)))

(define A (make-accumulator 5))

(check-eq? (A 10) 15)
(check-eq? (A 10) 25)

(define B (make-accumulator 1))

(check-eq? (B 2) 3)
(check-eq? (B 3) 6)
