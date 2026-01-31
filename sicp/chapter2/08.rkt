#lang sicp

(#%require rackunit)

(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(test-case
  "(sub-interval a b) result max value has to be a max minus b minus."
  (check-equal? (upper-bound (sub-interval (make-interval 1.0 5.0) (make-interval 2.0 4.0))) 3.0)
  (check-equal? (upper-bound (sub-interval (make-interval 1.0 5.0) (make-interval -2.0 -1.0))) 7.0))

(test-case
  "(sub-interval a b) result min value has to be a min minus b max."
  (check-equal? (lower-bound (sub-interval (make-interval 1.0 5.0) (make-interval 2.0 4.0))) -3.0)
  (check-equal? (lower-bound (sub-interval (make-interval -2.0 -1.0) (make-interval 1.0 3.0))) -5.0))