#lang sicp

(#%require rackunit)

(define (square x) (expt x 2))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

; the order of the result list is reversed because
; each next iteration points to a result of the previous one 
(check-equal? (square-list (list 1 2 3 4 5)) (list 25 16 9 4 1))