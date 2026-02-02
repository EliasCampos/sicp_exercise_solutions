#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (square-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (square tree)]
        [else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))]))

(check-equal?
 (square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
 '(1 (4 (9 16) 25) (36 49)))

(define (square-tree-2 tree)
  (if (null? tree)
      nil
      (map (lambda (sub-tree) (square-tree sub-tree)) tree)))

(check-equal?
 (square-tree-2
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
 '(1 (4 (9 16) 25) (36 49)))