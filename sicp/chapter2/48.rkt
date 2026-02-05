#lang sicp

(#%require rackunit)

(define (make-vect x y)
  (cons x y))

(define (make-segment start end)
  (list start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cadr segment))

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))
(define seg1 (make-segment v1 v2))

(check-equal? seg1 (list v1 v2))
(check-equal? (start-segment seg1) v1)
(check-equal? (end-segment seg1) v2)