#lang sicp

(#%require rackunit)

(define (union-set set1 set2)
  (cond [(null? set2) set1]
        [(null? set1) set2]
        [(= (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) (cdr set2)))]
        [(> (car set1) (car set2))
         (cons (car set2)
               (union-set set1 (cdr set2)))]
        [else (cons (car set1)
                    (union-set (cdr set1) set2))]))

(check-equal? (union-set '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-equal? (union-set '(1 2 3) '(2 3 4)) '(1 2 3 4))
(check-equal? (union-set '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6))
(check-equal? (union-set '(1 2) '()) '(1 2))
(check-equal? (union-set '() '(3 4)) '(3 4))