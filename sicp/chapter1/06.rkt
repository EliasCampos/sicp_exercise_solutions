#lang sicp

(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))

(define (new-if predicate then-clause else-clause)
  (cond [predicate then-clause]
        [else else-clause]))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

; Since the order of evaluation is applicative, that is,
; unless the procedure is a special form, the arguments are evaluated first and applied afterwards,
; the (sqrt-iter 1.0 2) would get stuck in an infinite loop if it were executed,
; due to constant evaluation of the recursive (sqrt-iter ...) procedure.