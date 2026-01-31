#lang sicp

(#%require rackunit)

(define (add-interval x y)
  (make-interval
   (+ (lower-bound x) (lower-bound y))
   (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval
     (min p1 p2 p3 p4)
     (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))


(test-case
  "(upper-bound x) must return the upper bound."
  (check-equal? (upper-bound (make-interval 6.12 7.48)) 7.48)
  (check-equal? (upper-bound (make-interval 2.97 2.58)) 2.97)
  (check-equal? (upper-bound (make-interval 3.0 3.0)) 3.0))
(test-case
  "(lower-bound x) must return the lower bound."
  (check-equal? (lower-bound (make-interval 6.12 7.48)) 6.12)
  (check-equal? (lower-bound (make-interval 2.97 2.58)) 2.58)
  (check-equal? (lower-bound (make-interval 3.0 3.0)) 3.0))

(test-case
  "(add-interval a b) result max value has to be the sum of two upper bounds."
  (check-equal? (upper-bound (add-interval (make-interval 2.0 4.0) (make-interval 1.0 5.0))) 9.0))
(test-case
  "(add-interval a b) result min value has to be the sum of two lower bounds."
  (check-equal? (lower-bound (add-interval (make-interval 2.0 4.0) (make-interval 1.0 5.0))) 3.0))

(test-case
  "(mul-interval a b) result max value has to be the maximum of the bound products."
  (check-equal? (upper-bound (mul-interval (make-interval 2.0 5.0) (make-interval 3.0 4.0))) 20.0))
(test-case
  "(mul-interval a b) result min value has to be the maximum of the bound products."
  (check-equal? (lower-bound (mul-interval (make-interval 2.0 5.0) (make-interval 3.0 4.0))) 6.0))

(test-case
  "(div a b) result has to be a multiplication of the first interval and a reciprocal of the second one."
  (check-equal? (lower-bound (div-interval (make-interval 2.0 10.0) (make-interval 4.0 8.0))) 0.25)
  (check-equal? (upper-bound (div-interval (make-interval 2.0 10.0) (make-interval 4.0 8.0))) 2.5))