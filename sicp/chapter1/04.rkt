#lang sicp

(#%require rackunit)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


(check-equal? (a-plus-abs-b 2 3) 5)
(check-equal? (a-plus-abs-b -2 3) 1)
(check-equal? (a-plus-abs-b 2 -3) 5)
(check-equal? (a-plus-abs-b -2 -3) 1)
