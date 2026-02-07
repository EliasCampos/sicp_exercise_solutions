#lang sicp

(#%require rackunit)


(define (element-of-set? x set)
  (cond [(null? set) false]
        [(equal? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [(element-of-set? (car set1) set2)
         (union-set (cdr set1) set2)]
        [else (cons (car set1) (union-set (cdr set1) set2))]))


(check-equal? (union-set '(1 2 3) '(2 3 4)) '(1 2 3 4))
(check-equal? (union-set '(a b c) '(c d e)) '(a b c d e))
(check-equal? (union-set '(1 2 3) '()) '(1 2 3))
(check-equal? (union-set '() '(a b c)) '(a b c))
(check-equal? (union-set '(1 2 3 (a b)) '(a b (a b) c)) '(1 2 3 a b (a b) c))