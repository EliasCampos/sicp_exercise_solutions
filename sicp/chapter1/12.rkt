#lang sicp

(#%require rackunit)

(define (bincoeff n k)
  (if (or (<= k 0) (>= k n))
      1
      (+ (bincoeff (- n 1) (- k 1))
         (bincoeff (- n 1) k))))

(test-case
  "Should return element of Pascal's triangle."
  (check-equal? (bincoeff 0 0) 1)
 
  (check-equal? (bincoeff 1 0) 1)
  (check-equal? (bincoeff 1 1) 1)

  (check-equal? (bincoeff 2 0) 1)
  (check-equal? (bincoeff 2 1) 2)
  (check-equal? (bincoeff 2 2) 1)

  (check-equal? (bincoeff 3 0) 1)
  (check-equal? (bincoeff 3 1) 3)
  (check-equal? (bincoeff 3 2) 3)
  (check-equal? (bincoeff 3 3) 1)

  (check-equal? (bincoeff 4 0) 1)
  (check-equal? (bincoeff 4 1) 4)
  (check-equal? (bincoeff 4 2) 6)
  (check-equal? (bincoeff 4 3) 4)
  (check-equal? (bincoeff 4 4) 1))