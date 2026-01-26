#lang sicp

(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (ferma-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ (random (- n 1)) 1)))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((ferma-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-time (- (runtime) start-time))))
(define (report-time elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (define (search-for-primes-step x)
    (timed-prime-test x)
    (search-for-primes (+ x 2) end))
  (cond ((even? start) (search-for-primes (+ start 1) end))
        ((not (> start end)) (search-for-primes-step start))))

(search-for-primes 1000 1019)
(search-for-primes 10000 10039)
(search-for-primes 1000000 1000039)