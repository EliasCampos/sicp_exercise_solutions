#lang sicp

(#%require sicp-pict)

(define (below-1 painter1 painter2)
  (let* ([split-point (make-vect 0.0 0.5)]
         [paint-bottom (transform-painter painter1
                                          (make-vect 0.0 0.0)
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.0 0.5))]
         [paint-top (transform-painter painter2
                                       (make-vect 0.0 0.5)
                                       (make-vect 1.0 0.5)
                                       (make-vect 0.0 1.0))])
    (lambda (frame)
      (paint-top frame)
      (paint-bottom frame))))

(paint (below-1 gray einstein))


(define (below-2 painter1 painter2)
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))

(paint (below-2 gray einstein))