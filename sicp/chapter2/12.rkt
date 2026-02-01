#lang sicp

(#%require rackunit)

(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

(define (center i)
(/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c pt)
  (let ([w (* c (/ pt 100))])
    (make-interval (- c w) (+ c w))))
(define (percent i)
  (let ([w (/ (- (upper-bound i) (lower-bound i)) 2)]
        [c (center i)])
    (* (/ w c) 100)))


(define-binary-check (check-approx-equal? actual expected)
  (let ([p 100])
    (= (/ (round (* actual p)) p) expected)))
(test-case
  "(make-center-percent c pt) intervals must be correct"
  (check-approx-equal? (lower-bound (make-center-percent 6.8 10)) 6.12)
  (check-approx-equal? (upper-bound (make-center-percent 6.8 10)) 7.48))
(test-case
  "(percent i) value must be correct"
  (check-approx-equal? (percent (make-interval 6.12 7.48)) 10))