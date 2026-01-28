#lang sicp

(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (repeated f n)
  (lambda (x)
    (define (repeated-iter k result)
      (if (<= k 0)
          result
          (repeated-iter (- k 1) (f result))))
     (repeated-iter n x)))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (nth-root x n)
  (define (f y) (/
                 x
                 (expt y (- n 1))))
  (let ((times (/ (log n) (log 2))))
    (fixed-point ((repeated average-damp times) f) 100.0)))

(nth-root 4 2)
(nth-root 16 4)
(nth-root 1024 10)

(nth-root 100 2)
(nth-root 10000 4)
(nth-root 1000000 6)

(nth-root 111 7)