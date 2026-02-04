#lang sicp

(#%require sicp-pict)

(define (split op1 op2)
  (lambda (painter n)
    (define (combine s)
      (op1 painter (op2 s s)))
    (define (walk i)
      (if (= i 0)
          painter
          (combine (walk (- i 1)))))
    (walk n)))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 3))
(paint (up-split einstein 3))