#lang sicp

(#%require rackunit)

(define (make-rat n d)
  (let ([g (gcd n d)] [k (if (< d 0) -1 1)])
    (cons (* k (/ n g)) (* k (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (mul-rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(test-case
  "Should make only nominator negative if rational number is negation."
  (check-equal? (mul-rat (make-rat 1 2) (make-rat -1 3)) (make-rat -1 6))
  (check-equal? (mul-rat (make-rat 1 -2) (make-rat 1 3)) (make-rat -1 6))
  (check-equal? (mul-rat (make-rat 1 -2) (make-rat -1 3)) (make-rat 1 6))
  (check-equal? (mul-rat (make-rat -2 1) (make-rat -3 1)) (make-rat 6 1))
  (check-equal? (mul-rat (make-rat 2 1) (make-rat 3 -1)) (make-rat -6 1)))