#lang sicp

(#%require rackunit)

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(= x (car set)) set]
        [(> (car set) x) (cons x set)]
        [else (cons (car set) (adjoin-set x (cdr set)))]))

(check-equal? (adjoin-set 6 '(1 3 5 7)) '(1 3 5 6 7))
(check-equal? (adjoin-set 4 '(2 4 6 8)) '(2 4 6 8))
(check-equal? (adjoin-set 1 '(1 2 3)) '(1 2 3))
(check-equal? (adjoin-set 4 '(2 3 4)) '(2 3 4))
(check-equal? (adjoin-set 5 '()) '(5))
(check-equal? (adjoin-set 9 '(2 4 6 8)) '(2 4 6 8 9))
(check-equal? (adjoin-set 1 '(2 4 6 8)) '(1 2 4 6 8))