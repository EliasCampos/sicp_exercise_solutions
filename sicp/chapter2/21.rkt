#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))


(test-case
  "Should return list of squared numbers."
  (check-equal? (square-list-1 (list 1 2 3 4 5)) (list 1 4 9 16 25))
  (check-equal? (square-list-1 (list 0 -1 -2 3 -4 5)) (list 0 1 4 9 16 25)))
(test-case
  "Should return list of squared numbers."
  (check-equal? (square-list-2 (list 1 2 3 4 5)) (list 1 4 9 16 25))
  (check-equal? (square-list-2 (list 0 -1 -2 3 -4 5)) (list 0 1 4 9 16 25)))