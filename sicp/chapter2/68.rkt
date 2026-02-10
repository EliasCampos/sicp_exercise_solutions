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

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define (here? elems)
    (cond [(null? elems)
           false]
          [(eq? symbol (car elems))
           true]
          [else (here? (cdr elems))]))
  (define (encode-iter t acc)
    (cond [(or (null? t) (leaf? t))
           (reverse acc)]
          [(here? (symbols (left-branch t)))
           (encode-iter (left-branch t) (cons 0 acc))]
          [(here? (symbols (right-branch t)))
           (encode-iter (right-branch t) (cons 1 acc))]
          [else (error "Tree has no symbol" symbol)]))
  (encode-iter tree '()))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(A D A B B C A))

(check-equal? (encode sample-message sample-tree)
              '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(check-exn
   #rx"[Tt]ree has no symbol"
   (lambda ()
     (encode '(A B X C) sample-tree)))