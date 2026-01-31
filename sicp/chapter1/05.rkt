#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; Since the order of evaluation is applicative,
; that is, the arguments are evaluated and then applied to a procedure,
; the test will get stuck in an infinite loop.
; (test 0 (p))
(test 0 (display "Applicative-order."))