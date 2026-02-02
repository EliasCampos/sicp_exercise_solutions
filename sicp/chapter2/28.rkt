#lang sicp

(#%require rackunit)

(define (fringe lst)
  (cond [(null? lst)
         nil]
        [(pair? (car lst))
         (append (fringe (car lst))
                 (fringe (cdr lst)))]
        [else
         (cons (car lst)
               (fringe (cdr lst)))]))

(define x (list (list 1 2) (list 3 4)))
(check-equal? (fringe x) (list 1 2 3 4))
(check-equal? (fringe (list x x)) (list 1 2 3 4 1 2 3 4))
(check-equal? (fringe (list (list 1 2) 3 (list 4 (list 5 6)))) (list 1 2 3 4 5 6))