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
(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(check-equal? (accumulate-n + 0 s) (list 22 26 30))
(check-equal? (accumulate-n * 7 '((-1 1 0) (10 20 30) (5 1 10))) (list -350 140 0))