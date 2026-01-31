#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (fast-expt b n)
  (define (fast-expt-iter x k a)
    (cond [(= k 0) a]
          [(even? k) (fast-expt-iter (square x) (/ k 2) a)]
          [else (fast-expt-iter x (- k 1) (* a x))]))
  (fast-expt-iter b n 1))


(test-case
  "Should implement exponentiation of number in given power."
  (check-equal? (fast-expt 1 0) 1)
  (check-equal? (fast-expt 42 0) 1)
  (check-equal? (fast-expt 42 1) 42)
  (check-equal? (fast-expt 3 2) 9)
  (check-equal? (fast-expt 2 5) 32)
  (check-equal? (fast-expt 5 3) 125))
