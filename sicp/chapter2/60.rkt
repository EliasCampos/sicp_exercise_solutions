#lang sicp

(#%require rackunit)


(define (element-of-set? x set)
  (cond [(null? set) false]
        [(equal? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2))
         '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))


(define S '(2 3 2 1 3 2 2))

(check-true (element-of-set? 'x (adjoin-set 'x S)))
(check-equal? (union-set S '(2 1 5 4))
              '(2 3 2 1 3 2 2 2 1 5 4))
(check-equal? (intersection-set S '(2 1 5 4))
              '(2 2 1 2 2))
