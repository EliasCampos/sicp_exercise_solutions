#lang sicp

(define (square x) (* x x))

(define (f g) (g 2))

(f square)
(f (lambda (n) (* n (+ n 1))))

(f f)