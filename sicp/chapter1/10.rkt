#lang sicp

(#%require rackunit)

(define (A x y)
  (cond [(= y 0) 0]
        [(= x 0) (* 2 y)]
        [(= y 1) 2]
        [else (A (- x 1) (A x (- y 1)))]))

(A 1 10)
(A 2 4)
(A 3 3)


(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

(test-case
  "(f n) must be 2+2+2+..+2 n times"
  (check-equal? (f 0) 0)
  (check-equal? (f 1) 2)
  (check-equal? (f 2) 4)
  (check-equal? (f 3) 6)
  (check-equal? (f 4) 8)
  (check-equal? (f 5) 10))

(test-case
  "(g n) must be 2*2*2*..*2 n times"
  (check-equal? (g 0) 0)
  (check-equal? (g 1) 2)
  (check-equal? (g 2) 4)
  (check-equal? (g 3) 8)
  (check-equal? (g 4) 16)
  (check-equal? (g 5) 32))

(test-case
  "(h n) must be 2^2^2^..^2 n times"
  (check-equal? (h 0) 0)
  (check-equal? (h 1) 2)
  (check-equal? (h 2) 4)
  (check-equal? (h 3) 16)
  (check-equal? (h 4) 65536))
