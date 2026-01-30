#lang sicp

(#%require rackunit)

(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (dissect num base count)
  (if (> (remainder num base) 0)
      count
      (dissect (/ num base) base (+ count 1))))

(define (my-car x) (dissect x 2 0))
(define (my-cdr x) (dissect x 3 0))

(test-case
  "car must return left element of the pair."
  (check-equal? (my-car (my-cons 0 0)) 0)
  (check-equal? (my-car (my-cons 1 2)) 1)
  (check-equal? (my-car (my-cons 9 6)) 9)
  (check-equal? (my-car (my-cons 5 5)) 5)
  (check-equal? (my-car (my-cons 7 4)) 7))
(test-case
  "crd must return right element of the pair."
  (check-equal? (my-cdr (my-cons 0 0)) 0)
  (check-equal? (my-cdr (my-cons 1 2)) 2)
  (check-equal? (my-cdr (my-cons 9 6)) 6)
  (check-equal? (my-cdr (my-cons 5 5)) 5)
  (check-equal? (my-cdr (my-cons 7 4)) 4))