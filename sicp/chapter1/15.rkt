#lang sicp

(#%require rackunit)

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3)))))

(sine 12.15)

(define (p-times x n)
  (if (not (> (abs x) 0.1))
       n
       (p-times (/ x 3) (+ n 1))))
(check-equal? (p-times 12.15 0) 5)
