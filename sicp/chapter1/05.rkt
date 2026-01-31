#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; Since the order of evaluation is applicative, that is,
; the arguments are evaluated before they are applied to a procedure,
; the (test 0 (p)) would get stuck in an infinite loop if it were executed.
(test 0 (display "Applicative-order."))