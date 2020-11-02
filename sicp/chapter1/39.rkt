#lang sicp

(#%require racket/math)
(#%require rackunit)


(define (cont-frac N D k)
  (define (cont-frac-iter i a)
    (if (= i 0)
        a
        (cont-frac-iter (- i 1) (/ (N i) (+ (D i) a)))))
  
  (cont-frac-iter k 0))
(define (tan-cf x precision)
  (define (^2 num) (* num num))
  (cont-frac (lambda (k) (if (= k 1) x (* -1 (^2 x)))) (lambda (k) (- (* 2 k) 1)) precision))


(define-binary-check (check-in-tolerance actual expected)
  (< (abs (- actual expected)) 0.001))
(test-case
  "Should calculate tangens of angle."
  (check-in-tolerance (tan-cf 0 100) 0)
  (check-in-tolerance (tan-cf pi 100) 0)
  (check-in-tolerance (tan-cf (/ pi 4) 100) 1.0)
  (check-in-tolerance (tan-cf 1 100) 1.55740))
