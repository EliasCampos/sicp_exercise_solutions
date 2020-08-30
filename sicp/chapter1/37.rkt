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
(define (eulers-num precision)
  (define (divides-by-3? num) (= (remainder num 3) 0))
  (define (coeff-seq k)
    (let ([i (+ k 1)])
      (if (divides-by-3? i)
          (* (/ i 3) 2)
          1)))

  (+ 2 (cont-frac (lambda (_) 1.0) coeff-seq precision)))
(define (tan-cf x precision)
  (define (^2 num) (* num num))
  (cont-frac (lambda (k) (if (= k 1) x (* -1 (^2 x)))) (lambda (k) (- (* 2 k) 1)) precision))


(define-binary-check (check-in-tolerance actual expected)
  (< (abs (- actual expected)) 0.001))

(define GOLDEN_RATIO 1.6180339887)
(define EULERS_NUM (exp 1))
(test-case
  "Should implement common continue fraction."
  (check-in-tolerance (golden-ratio 100) GOLDEN_RATIO)
  (check-in-tolerance (eulers-num 100) EULERS_NUM))

(test-case
  "Should calculate tangens of angle."
  (check-in-tolerance (tan-cf 0 100) 0)
  (check-in-tolerance (tan-cf pi 100) 0)
  (check-in-tolerance (tan-cf (/ pi 4) 100) 1.0)
  (check-in-tolerance (tan-cf 1 100) 1.55740))