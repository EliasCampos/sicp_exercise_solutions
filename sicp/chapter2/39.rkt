#lang sicp

(#%require rackunit)

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse-2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(check-equal? (reverse-1 (list 1 2 3 4 5)) (list 5 4 3 2 1))
(check-equal? (reverse-2 (list 1 2 3 4 5)) (list 5 4 3 2 1))
(check-equal? (reverse-1 (list 1 2 4 8 16)) (list 16 8 4 2 1))
(check-equal? (reverse-2 (list 1 2 4 8 16)) (list 16 8 4 2 1))