#lang sicp

(#%require rackunit)


(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define p1 (cons 'A '()))
(define p2 (cons 'B p1))
(define p3 (cons 'C p2))

(check-eq? (count-pairs p3) 3)

(set-car! p3 p1)
(check-eq? (count-pairs p3) 4)

(set-car! p2 p1)
(set-car! p3 p2)
(check-eq? (count-pairs p3) 7)

(set-car! p3 p3)
; If I were to call (count-pairs p3) next,
; the function would never return a result
; due to an infinite self-reference