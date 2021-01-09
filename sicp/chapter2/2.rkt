#lang sicp

(#%require rackunit)

(define (make-segment start-point end-point) (cons start-point end-point))
(define (midpoint-segment seg)
  (define (avg x y) (/ (+ x y) 2))
  (let ([s (start-segment seg)] [e (end-segment seg)])
    (make-point (avg (x-point s) (x-point e)) (avg (y-point s) (y-point e)))))

(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(test-case
  "Should return appropriate X-coordinate of the point."
  (check-equal? (x-point (make-point 3 2)) 3)
  (check-equal? (x-point (make-point -1 10)) -1))
(test-case
  "Should return appropriate Y-coordinate of the point."
  (check-equal? (y-point (make-point 3 2)) 2)
  (check-equal? (y-point (make-point 1 -10)) -10))

(test-case
  "Should return a middle point of the segment."
  (check-equal? (midpoint-segment (make-segment (make-point 1 1) (make-point 5 9))) (make-point 3 5))
  (check-equal? (midpoint-segment (make-segment (make-point -7 -2) (make-point 7 2))) (make-point 0 0)))
