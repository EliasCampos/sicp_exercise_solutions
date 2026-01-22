#lang sicp

(#%require rackunit)

(define (reverse lst)
  (define (reverse-iter curr result)
    (if (null? curr)
        result
        (reverse-iter (cdr curr) (cons (car curr) result))))
  (reverse-iter lst nil))
        

(test-case
  "Should return reversed list."
  (check-equal? (reverse (list 1 4 9 16 25)) (list 25 16 9 4 1))
  (check-equal? (reverse (list 1 0 -1)) (list -1 0 1))
  (check-equal? (reverse nil) nil))