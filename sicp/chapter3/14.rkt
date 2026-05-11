#lang sicp

(#%require rackunit)

(define (mystery a)
  (define (loop x y)
    (cond [(null? x) y]
          [else (let ([temp (cdr x)])
                  (set-cdr! x y)
                  (loop temp x))]))
  (loop a '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

(check-equal? w (list 'd 'c 'b 'a))
