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


(define alphabet '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define song-tree (generate-huffman-tree alphabet))
(define song '(GET A JOB SHA NA NA NA NA NA NA NA NA
              GET A JOB SHA NA NA NA NA NA NA NA NA
              WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
              SHA BOOM))
(define encoded-msg (encode song song-tree))

(check-equal? (length encoded-msg) (+ 5 4 5 4 1 1 1 1 1 1 1 1
                                      5 4 5 4 1 1 1 1 1 1 1 1
                                      5 2 2 2 2 2 2 2 2 2
                                      4 5))
(check-true (< (length encoded-msg) (* (length song) 3)))
