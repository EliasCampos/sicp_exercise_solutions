#lang sicp

(#%require rackunit)

(define (expand-subs-by-val subs val)
  (append subs
          (map (lambda (sub) (cons val sub)) subs)))
(define (subsets s)
  (if (null? s)
      (list nil)
      (expand-subs-by-val (subsets (cdr s)) (car s))))

(check-equal? (subsets nil)
              '(()))
(check-equal? (subsets (list 1))
              '(() (1)))
(check-equal? (subsets (list 1 2 3))
              '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))