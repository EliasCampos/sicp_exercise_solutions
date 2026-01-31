#lang sicp

(#%require rackunit)

(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(test-case
  "(interval-width x) must be half a difference between the upper and the lower bound."
  (check-equal? (interval-width (make-interval 1.0 5.0)) 2.0)
  (check-equal? (interval-width (make-interval -5.0 5.0)) 5.0))

(define (interval-sum-width-1 x y)
  (interval-width (add-interval x y)))
(define (interval-sum-width x y)
  (+ (interval-width x) (interval-width y)))

(define test-interval-1 (make-interval 1.0 5.0))
(define test-interval-2 (make-interval -2.0 3.0))
(define test-interval-3 (make-interval -5.0 5.0))

; sum(a,b) = (min(a) + min(b)) ... (max(a) + max(b))
; width(x) = (max(x) - min(x)) / 2
; width(sum(a,b)) = [max(sum(a,b)) - min(sum(a,b))] / 2 = [(max(a) + max(b)) - (min(a) + min(b))] / 2 =
; = [(max(a) - min(a)) + (max(b) - min(b))] / 2 = width(a) + width(b)
(test-case
  "width(sum(a,b)) must be equal to width(a) + width(b) and thereby a function of the widths."
  (check-equal? (interval-width (add-interval test-interval-1 test-interval-2))
                (+ (interval-width test-interval-1) (interval-width test-interval-2)))
  (check-equal? (interval-width (add-interval test-interval-2 test-interval-3))
                (+ (interval-width test-interval-2) (interval-width test-interval-3)))
  (check-equal? (interval-width (add-interval test-interval-1 test-interval-3))
                (+ (interval-width test-interval-1) (interval-width test-interval-3))))

; diff(a,b) = (min(a) - max(b)) ... (max(a) - min(b))
; width(x) = (max(x) - min(x)) / 2
; width(diff(a,b)) = [max(diff(a,b)) - min(diff(a,b))] / 2 = [(max(a) - min(b)) - (min(a) - max(b))] / 2 =
; = [(max(a) - min(a)) + (max(b) - min(b))] / 2 = width(a) + width(b)
(test-case
  "width(diff(a,b)) must be equal to width(a) + width(b) and thereby a function of the widths."
  (check-equal? (interval-width (sub-interval test-interval-1 test-interval-2))
                (+ (interval-width test-interval-1) (interval-width test-interval-2)))
  (check-equal? (interval-width (sub-interval test-interval-2 test-interval-3))
                (+ (interval-width test-interval-2) (interval-width test-interval-3)))
  (check-equal? (interval-width (sub-interval test-interval-1 test-interval-3))
                (+ (interval-width test-interval-1) (interval-width test-interval-3))))