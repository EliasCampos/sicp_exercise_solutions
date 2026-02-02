#lang sicp

(#%require rackunit)

(define (make-binary-mobile left-b right-b)
  (list left-b right-b))
(define (make-branch length structure)
  (list length structure))

(define (left-branch binary-mobile)
  (car binary-mobile))
(define (right-branch binary-mobile)
  (cadr binary-mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))


(define br11 (make-branch 10 6))
(define br12 (make-branch 5 8))
(define bm1 (make-binary-mobile br11 br12))
(define br21 (make-branch 15 4))
(define br22 (make-branch 5 bm1))
(define bm2 (make-binary-mobile br21 br22))

(test-case
  "Left branch."
  (check-equal? (left-branch bm1) br11)
  (check-equal? (left-branch bm2) br21))
(test-case
  "Right branch."
  (check-equal? (right-branch bm1) br12)
  (check-equal? (right-branch bm2) br22))
(test-case
  "Branch length."
  (check-equal? (branch-length br11) 10)
  (check-equal? (branch-length br12) 5)
  (check-equal? (branch-length br21) 15)
  (check-equal? (branch-length br22) 5))
(test-case
  "Branch structure."
  (check-equal? (branch-structure br11) 6)
  (check-equal? (branch-structure br12) 8)
  (check-equal? (branch-structure br21) 4)
  (check-equal? (branch-structure br22) bm1))