#lang sicp

(#%require rackunit)

(define (sum-squares-of-greatest-pair x y z)
  (cond [(and (< x y) (< x z))
         (+ (* y y) (* z z))]
        [(and (< y x) (< y z))
         (+ (* x x) (* z z))]
        [else (+ (* x x) (* y y))]))

(test-case
  "Should return sum of squares of two greatest numbers."
  (check-eq? (sum-squares-of-greatest-pair 0 0 0) 0)
  (check-eq? (sum-squares-of-greatest-pair 1 1 1) 2)
  (check-eq? (sum-squares-of-greatest-pair 2 2 2) 8)
  (check-eq? (sum-squares-of-greatest-pair 1 2 3) (+ 4 9))
  (check-eq? (sum-squares-of-greatest-pair 3 2 1) (+ 9 4))
  (check-eq? (sum-squares-of-greatest-pair 5 -9 2) (+ 25 4)))
