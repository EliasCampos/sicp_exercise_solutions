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
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set))) (cons x set)]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
       (adjoin-set (make-leaf (caar pairs) ; symbol
                              (cadar pairs)) ; frequency
                   (make-leaf-set (cdr pairs)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leafs)
  (cond [(null? leafs)
         '()]
        [(null? (cdr leafs))
         (car leafs)]
        [else (successive-merge (adjoin-set (make-code-tree (car leafs)
                                                            (cadr leafs))
                                            (cddr leafs)))]))


(check-equal? (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
              (make-code-tree
               (make-leaf 'A 4)
               (make-code-tree
                (make-leaf 'B 2)
                (make-code-tree
                 (make-leaf 'D 1)
                 (make-leaf 'C 1)))))
