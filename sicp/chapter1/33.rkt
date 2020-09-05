#lang sicp

(#%require rackunit)
(#%require math)

(define (filtered-accumulate combiner null-value term a next b filterer)
  (define (filtered-accumulate-iter x result)
    (if (> x b)
        result
       (filtered-accumulate-iter (next x) (if (filterer x) (combiner (term x) result) result))))
  
  (filtered-accumulate-iter a null-value))

(define (square x) (* x x))
(define (sum-of-prime-squares n)
  (filtered-accumulate + 0 square 2 inc n prime?))

(test-case
  "Should calculate sum of squares of a prime number."
  (check-equal? (sum-of-prime-squares 10) (+ 4 9 25 49))
  (check-equal? (sum-of-prime-squares 15) (+ 4 9 25 49 121 169)))


(define (product-of-co-primes n)
  (filtered-accumulate * 1 abs 1 inc n (lambda (i) (and (< i n) (= (gcd i n) 1)))))

(test-case
  "Should calculate product of all positive numbers that co-prime with a given."
  (check-equal? (product-of-co-primes 10) (* 3 7 9))
  (check-equal? (product-of-co-primes 5) (* 2 3 4)))