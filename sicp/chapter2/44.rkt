#lang sicp

(#%require sicp-pict)

(define (up-split painter n)
  (define (combine s)
    (below painter (beside s s)))
  (define (walk i)
    (if (= i 0)
        painter
        (combine (walk (- i 1)))))
  (walk n))

(paint (up-split einstein 3))