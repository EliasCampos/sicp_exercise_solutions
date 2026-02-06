#lang sicp

(#%require rackunit)

(define (my-equal? a b)
  (or (and (not (pair? a))
           (not (pair? b))
           (eq? a b))
      (and (pair? a)
           (pair? b)
           (or (and (null? a)
                    (null? b))
               (and (not (null? a))
                    (not (null? b))
                    (my-equal? (car a) (car b))
                    (my-equal? (cdr a) (cdr b)))))))

(check-true (my-equal? 'a 'a))
(check-false (my-equal? 'a 'b))
(check-false (my-equal? 'a '(a b)))
(check-true (my-equal? '(this is a list) '(this is a list)))
(check-false (my-equal? '(this is a list) '(this (is a) list)))
(check-true (my-equal? '(this (is a) list) '(this (is a) list)))
(check-true (my-equal? '(a (b (c d) e) f) '(a (b (c d) e) f)))
