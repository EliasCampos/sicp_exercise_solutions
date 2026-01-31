#lang sicp

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

(define (prime? n) (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-time (- (runtime) start-time))
      (newline)))
(define (report-time elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes start end)
  (define (search-for-primes-step x)
    (timed-prime-test x)
    (search-for-primes (+ x 2) end))
  (define (finalize-iter)
    (display "---DONE---")
    (newline))
  (cond [(even? start) (search-for-primes (+ start 1) end)]
        [(not (> start end)) (search-for-primes-step start)]
        [else (finalize-iter)]))

(search-for-primes 1000 1019)
(search-for-primes 10000 10039)
(search-for-primes 1000000 1000039)