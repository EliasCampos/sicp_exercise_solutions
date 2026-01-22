#lang sicp

(#%require rackunit)

(define (last-pair lst)
  (let ([rest (cdr lst)])
    (if (null? rest)
        (list (car lst))
        (last-pair rest))))

(test-case
  "Should return list with one item - last element of base list."
  (check-equal? (last-pair (list 23 72 149 34)) (list 34))
  (check-equal? (last-pair (list 42)) (list 42)))