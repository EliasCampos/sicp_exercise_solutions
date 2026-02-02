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

(define (is-binary-mobile? value)
  (pair? value))

(define (total-weight binary-mobile)
  (let ([left-struct (branch-structure (left-branch binary-mobile))]
        [right-struct (branch-structure (right-branch binary-mobile))])
       (+ (struct-weight left-struct)
          (struct-weight right-struct))))
(define (struct-weight structure)
  (if (is-binary-mobile? structure)
      (total-weight structure)
      structure))

(define (is-binary-mobile-balanced? binary-mobile)
  (let* ([left-br (left-branch binary-mobile)]
         [right-br (right-branch binary-mobile)]
         [left-len (branch-length left-br)]
         [right-len (branch-length right-br)]
         [left-struct (branch-structure left-br)]
         [right-struct (branch-structure right-br)])
    (and (= (* left-len (struct-weight left-struct))
            (* right-len (struct-weight right-struct)))
         (or (not (is-binary-mobile? left-struct))
             (is-binary-mobile-balanced? left-struct))
         (or (not (is-binary-mobile? right-struct))
             (is-binary-mobile-balanced? right-struct)))))


(define br11 (make-branch 10 4))
(define br12 (make-branch 5 8))
(define bm1 (make-binary-mobile br11 br12))
(define br21 (make-branch 30 2))
(define br22 (make-branch 5 bm1))
(define bm2 (make-binary-mobile br21 br22))
(define br31 (make-branch 13 13))
(define br32 (make-branch 7 7))
(define bm3 (make-binary-mobile br31 br32))

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
  (check-equal? (branch-length br21) 30)
  (check-equal? (branch-length br22) 5))
(test-case
  "Branch structure."
  (check-equal? (branch-structure br11) 4)
  (check-equal? (branch-structure br12) 8)
  (check-equal? (branch-structure br21) 2)
  (check-equal? (branch-structure br22) bm1))

(test-case
  "Total weight."
  (check-equal? (total-weight bm1) 12)
  (check-equal? (total-weight bm2) 14))

(test-case
 "Balance check."
 (check-true (is-binary-mobile-balanced? bm1))
 (check-true (is-binary-mobile-balanced? bm2))
 (check-false (is-binary-mobile-balanced? bm3)))