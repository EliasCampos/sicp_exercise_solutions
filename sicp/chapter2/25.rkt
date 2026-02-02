#lang sicp

(#%require rackunit)

(define A1 '(1 3 (5 7) 9))
(define A2 '((7)))
(define A3 '(1 (2 (3 (4 (5 (6 7)))))))

(check-eq? (cadr (cadr (cdr A1))) 7)
(check-eq? (caar A2) 7)
(check-eq? (cadar (cdadr (cadar (cdadr A3)))) 7)