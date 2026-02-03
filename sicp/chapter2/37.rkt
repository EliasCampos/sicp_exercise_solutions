#lang sicp

(#%require rackunit)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))
(define (transpose mat)
  (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (w) (matrix-*-vector cols w)) m)))

(define v1 '(5 10))
(define v2 '(-3 3))
(define m1 '((1 2) (3 4)))
(define m2 '((-5 1) (-1 5)))

(check-equal? (dot-product v1 v2) 15)
(check-equal? (matrix-*-vector m1 v1) (list 25 55))
(check-equal? (transpose m1) '((1 3) (2 4)))
(check-equal? (matrix-*-matrix m1 m2) '((-7 11) (-19 23)))