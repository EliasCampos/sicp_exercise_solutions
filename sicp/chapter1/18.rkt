#lang sicp

(#%require rackunit)

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (define (fast-mul-iter x y acc)
    (cond [(= y 0) acc]
          [(even? y) (fast-mul-iter (double x) (halve y) acc)]
          [else (fast-mul-iter x (- y 1) (+ acc x))]))

  (fast-mul-iter a b 0))


(test-case
  "Should implement fast multiplication."
  (check-equal? (fast-mul 1 0) 0)
  (check-equal? (fast-mul 42 0) 0)
  (check-equal? (fast-mul 42 1) 42)
  (check-equal? (fast-mul 3 2) 6)
  (check-equal? (fast-mul 2 5) 10)
  (check-equal? (fast-mul 7 7) 49))
