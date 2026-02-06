#lang sicp

(#%require sicp-pict)

(define (flip-horizontally painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
(paint (flip-horizontally einstein))