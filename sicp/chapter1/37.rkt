#lang sicp

(#%require racket/math)
(#%require rackunit)


(define (cont-frac N D k)
  (define (cont-frac-iter i a)
    (if (= i 0)
        a
        (cont-frac-iter (- i 1) (/ (N i) (+ (D i) a)))))
  
  (cont-frac-iter k 0))

(define (golden-ratio precision)
  (/ 1 (cont-frac (lambda (_) 1.0) (lambda (_) 1.0) precision)))

(define-binary-check (check-in-tolerance actual expected)
  (< (abs (- actual expected)) 0.001))

(define GOLDEN_RATIO 1.6180339887)
(test-case
  "Should implement common continue fraction."
  (check-in-tolerance (golden-ratio 100) GOLDEN_RATIO))