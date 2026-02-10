#lang sicp

(#%require rackunit)

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 current-bits current-branch)
    (if (null? current-bits)
        '()
        (decode-step current-bits current-branch)))
  (define (decode-step current-bits current-branch)
    (let ([next-branch (choose-branch (car current-bits) current-branch)])
      (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr current-bits) tree))
          (decode-1 (cdr current-bits) next-branch))))
  (decode-1 bits tree))
(define (choose-branch bit tree)
  (cond [(= bit 0) (left-branch tree)]
        [(= bit 1) (right-branch tree)]
        [else (error "bad bit: CHOOSE-BRANCH" bit)]))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(check-equal? (decode sample-message sample-tree)
              '(A D A B B C A))
