#lang sicp

(#%require rackunit)

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-rect p1 p2)
  (let ([x1 (x-point p1)]
        [y1 (y-point p1)]
        [x2 (x-point p2)]
        [y2 (y-point p2)])
    (cons (abs (- x1 x2)) (abs (- y1 y2)))))
(define (make-rect2 width height)
  (cons width height))

(define (horizontal-size rect)
  (car rect))
(define (vertical-size rect)
  (cdr rect))

(define (rect-perimeter rect)
  (* (+ (horizontal-size rect) (vertical-size rect)) 2))
(define (rect-area rect)
  (* (horizontal-size rect) (vertical-size rect)))

(test-case
  "Area of a rectangle."
  (check-equal? (rect-area (make-rect (make-point 1 4) (make-point 5 1))) 12)
  (check-equal? (rect-area (make-rect2 3 4)) 12)
  (check-equal? (rect-area (make-rect (make-point 5 -2) (make-point -5 2))) 40)
  (check-equal? (rect-area (make-rect2 10 4)) 40))
(test-case
  "Perimeter of a rectangle."
  (check-equal? (rect-perimeter (make-rect (make-point 1 4) (make-point 5 1))) 14)
  (check-equal? (rect-perimeter (make-rect2 3 4)) 14)
  (check-equal? (rect-perimeter (make-rect (make-point 5 -2) (make-point -5 2))) 28)
  (check-equal? (rect-perimeter (make-rect2 10 4)) 28))
