#lang sicp

(#%require rackunit)

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))


(test-case
  "Should return c for x = 0."
  (check-equal? ((cubic 1 2 3) 0) 3)
  (check-equal? ((cubic 7 7 1) 0) 1)
   (check-equal? ((cubic 5 6 0) 0) 0))


(test-case
  "Should return 1 + a + b + c in case when x = 1"
  (check-equal? ((cubic 1 2 3) 1) (+ 1 1 2 3))
  (check-equal? ((cubic 0 0 0) 1) 1)
  (check-equal? ((cubic 5 0 7) 1) (+ 1 5 7)))

(test-case
  "Should return -1 + a - b + c in case when x = -1"
  (check-equal? ((cubic 1 2 3) -1) (+ -1 1 -2 3))
  (check-equal? ((cubic 5 0 7) -1) (+ -1 5 0 7)))
