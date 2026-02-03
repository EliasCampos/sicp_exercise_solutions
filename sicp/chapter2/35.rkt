#lang sicp

(#%require rackunit)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (n)
          (if (pair? n)
              (count-leaves n)
              1))
        t)))


(define x (cons (list 1 2) (list 3 4)))
(check-equal? (count-leaves x) 4)
(check-equal? (count-leaves (list x x)) 8)
(check-equal? (count-leaves (list x x 7)) 9)