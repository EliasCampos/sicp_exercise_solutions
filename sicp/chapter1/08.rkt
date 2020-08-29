#lang sicp

(#%require rackunit)

(define PRECISION 0.001)

(define (cbrt x)
  (define (good-enough? guess)
    (let ([rel-delta (/ (abs (- (* guess guess guess) x)) x)])
      (<= rel-delta PRECISION)))
  (define (improve y)
    (/ (+ (/ x (* y y)) (* 2 y)) 3))
  (define (cbrt-iter guess)
    (if (good-enough? guess)
        guess
        (cbrt-iter (improve guess))))
 
  (if (= x 0) 0 (cbrt-iter 1.0)))


(define-binary-check (check-in-tolerance actual expected)
  (<= (abs (- actual expected)) (* expected PRECISION)))

(test-case
  "Should return cubic root of number."
  (check-in-tolerance (cbrt 0.0) 0.0)
  (check-in-tolerance (cbrt 1.0) 1.0)
  (check-in-tolerance (cbrt 27.0) 3.0)
  (check-in-tolerance (cbrt 1000.0) 10.0)
  (check-in-tolerance (cbrt 100.0) 4.641))
