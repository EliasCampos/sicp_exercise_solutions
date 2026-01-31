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


(define (cont-frac-b n d k)
  (define (cont-frac-step i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (cont-frac-step (+ i 1))))))
  (cont-frac-step 1))

(test-case
  "The recurcive procedure with n(i) = 1 and d(i) = 1 has to calcuale 1 / phi."
  (check-in-tolerance
   (cont-frac-b (lambda (_) 1.0)
                (lambda (_) 1.0)
                20)
   (- GOLDEN_RATIO 1)))
