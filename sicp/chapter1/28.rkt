#lang sicp

(#%require rackunit)

(define (square x) (* x x))
(define (square-check x n m)
  (cond [(and (> x 1) (< x n) (= (remainder (square x) m) 1))
         0]
        [else (square x)]))
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder
          (square-check (expmod base (/ exp 2) m) exp m)
          m)]
        [else
         (remainder
          (* base (expmod base (- exp 1) m))
          m)]))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ (random (- n 1)) 1)))
(define (fast-prime? n)
  (define (fast-prime-test times)
    (cond [(= times 0) true]
          [(miller-rabin-test n) (fast-prime-test (- times 1))]
          [else false]))
  (fast-prime-test 100))

(test-case
  "The Miller-Rabit test has to return true for prime numbers."
  (check-true (fast-prime? 2))
  (check-true (fast-prime? 3))
  (check-true (fast-prime? 5))
  (check-true (fast-prime? 7))
  (check-true (fast-prime? 101))
  (check-true (fast-prime? 103))
  (check-true (fast-prime? 199))
  (check-true (fast-prime? 233))
  (check-true (fast-prime? 1009))
  (check-true (fast-prime? 7919)))

(test-case
  "The Miller-Rabit test has to return false for composite numbers."
  (check-false (fast-prime? 4))
  (check-false (fast-prime? 6))
  (check-false (fast-prime? 10))
  (check-false (fast-prime? 12))
  (check-false (fast-prime? 15))
  (check-false (fast-prime? 104))
  (check-false (fast-prime? 1011))
  (check-false (fast-prime? 1915))
  (check-false (fast-prime? 5703))

  (check-false (fast-prime? 561))
  (check-false (fast-prime? 1105))
  (check-false (fast-prime? 1729))
  (check-false (fast-prime? 2465))
  (check-false (fast-prime? 2821))
  (check-false (fast-prime? 6601)))