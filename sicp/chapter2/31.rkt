#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (tree-map proc tree)
  (cond [(null? tree)
         nil]
        [(not (pair? tree))
         (proc tree)]
        [else
         (map (lambda (sub-tree)
                (tree-map proc sub-tree))
              tree)]))

(define (square-tree tree)
  (tree-map square tree))

(check-equal?
 (square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
 '(1 (4 (9 16) 25) (36 49)))