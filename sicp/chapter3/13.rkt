#lang sicp

(#%require rackunit)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(check-equal? (car z) 'a)
(check-equal? (cadr z) 'b)
(check-equal? (caddr z) 'c)
(check-equal? (cadddr z) 'a)  ; if I were to call (last-pair z), it would get stuck in an infinite loop
