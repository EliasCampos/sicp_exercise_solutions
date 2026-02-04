#lang sicp

(#%require rackunit)

(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(test-case
 "(add-vect v1 v2) must be (x1 , y1) + (x2 , y2) = (x1 + x2 , y1 + y2)"
 (check-equal? (add-vect (make-vect 1 2) (make-vect 3 -5)) (make-vect 4 -3))
 (check-equal? (add-vect (make-vect -3 5) (make-vect 7 -1)) (make-vect 4 4))
 (check-equal? (add-vect (make-vect 11 13) (make-vect -11 -13)) (make-vect 0 0)))
(test-case
 "(sub-vect v1 v2) must be (x1 , y1) - (x2 , y2) = (x1 - x2 , y1 - y2)"
 (check-equal? (sub-vect (make-vect 1 2) (make-vect 3 -5)) (make-vect -2 7))
 (check-equal? (sub-vect (make-vect 4 -5) (make-vect 7 -1)) (make-vect -3 -4))
 (check-equal? (sub-vect (make-vect 11 13) (make-vect 11 13)) (make-vect 0 0)))
(test-case
 "(scale-vect s v) must be s · (x , y) = (sx , sy)"
 (check-equal? (scale-vect 5 (make-vect 1 2)) (make-vect 5 10))
 (check-equal? (scale-vect 3 (make-vect 2 2)) (make-vect 6 6))
 (check-equal? (scale-vect 10 (make-vect 0 -5)) (make-vect 0 -50)))