#lang sicp

(#%require rackunit)

(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        [else (+ (cc amount
                     (except-first-denominator coin-values))
                 (cc (- amount (first-denominator
                                coin-values))
                     coin-values))]))
(define (no-more? coin-values)
  (null? coin-values))
(define (first-denominator coin-values)
  (car coin-values))
(define (except-first-denominator coin-values)
  (cdr coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 25 10 5 1 0.5))

(check-eq? (cc 0 us-coins) 1)
(check-eq? (cc 1 us-coins) 1)
(check-eq? (cc 100 us-coins) 292)
(check-eq? (cc 0 uk-coins) 1)
(check-eq? (cc 1 uk-coins) 2)
(check-eq? (cc 5 uk-coins) 7)