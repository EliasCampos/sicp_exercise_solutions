#lang sicp

(#%require sicp-pict)

(define (up-split painter n)
  (define (step i)
    (let ([s (walk (- i 1))])
        (below painter (beside s s))))
    (define (walk i)
      (if (= i 0) painter (step i)))
  (walk n))

(paint (up-split einstein 3))