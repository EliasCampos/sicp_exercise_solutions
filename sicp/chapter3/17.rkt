#lang sicp

(#%require rackunit)

(define (count-pairs x)
  (define checked '())
  (define (any pred l)
    (cond [(null? l) false]
          [(pred (car l)) true]
          [else (any pred (cdr l))]))
  (define (checked? el)
    (any (lambda (i) (eq? el i)) checked))
  (define (walk-count el)
    (cond [(not (pair? el)) 0]
          [(checked? el) 0]
          [else
           (set! checked (append checked (list el)))
           (+ (walk-count (car el))
              (walk-count (cdr el))
              1)]))
  (walk-count x))


(define p1 (cons 'A '()))
(define p2 (cons 'B p1))
(define p3 (cons 'C p2))

(check-eq? (count-pairs p3) 3)

(set-car! p3 p1)
(check-eq? (count-pairs p3) 3)

(set-car! p2 p1)
(set-car! p3 p2)
(check-eq? (count-pairs p3) 3)

(set-car! p3 p3)
(check-eq? (count-pairs p3) 3)
