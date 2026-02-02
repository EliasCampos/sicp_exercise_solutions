#lang sicp

(#%require rackunit)

(define (deep-reverse lst)
  (define (reverse-step curr result)
    (if (null? curr)
        result
        (reverse-step (cdr curr)
                      (cons (if (pair? (car curr))
                                (reverse-step (car curr) nil)
                                (car curr))
                            result))))
  (reverse-step lst nil))

(define x (list (list 1 2) (list 3 4)))
(check-equal? (deep-reverse x) (list (list 4 3) (list 2 1)))
(check-equal? (deep-reverse (list 1 (list 2 3) 4 (list (list 5 6) (list 7 8))))
              (list (list (list 8 7) (list 6 5)) 4 (list 3 2) 1))