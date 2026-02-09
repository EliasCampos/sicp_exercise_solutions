#lang sicp

(#%require rackunit)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree e left right)
  (list e left right))

(define (tree->list tree)
  (define (copy-to-list t result-list)
    (if (null? t)
        result-list
        (copy-to-list (left-branch t)
                      (cons (entry t)
                            (copy-to-list
                             (right-branch t)
                             result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (walk elts n)))
  (define (walk elts n)
    (let* ([left-size (quotient (- n 1) 2)]
           [left-result (partial-tree elts left-size)]
           [left-tree (car left-result)]
           [non-left-elts (cdr left-result)]
           [right-size (- n (+ left-size 1))]
           [this-entry (car non-left-elts)]
           [right-elts (cdr non-left-elts)]
           [right-result (partial-tree right-elts right-size)]
           [right-tree (car right-result)]
           [remaining-elements (cdr right-result)])
      (cons (make-tree this-entry
                       left-tree
                       right-tree)
            remaining-elements)))  
  (car (partial-tree elements (length elements))))


(define (intersection-set set1 set2)
  (define (intersection-list l1 l2)
    (if (or (null? l1) (null? l2))
        '()
        (walk l1 l2)))
  (define (walk l1 l2)
    (let ([x1 (car l1)]
          [x2 (car l2)])
      (cond [(< x1 x2)
             (intersection-list (cdr l1) l2)]
            [(< x2 x1)
             (intersection-list l1 (cdr l2))]
            [else
             (cons x1 (intersection-list (cdr l1)
                                         (cdr l2)))])))
  (if (or (null? set1) (null? set2))
      '()
      (list->tree (intersection-list (tree->list set1)
                                     (tree->list set2)))))


(define (union-set set1 set2)
  (define (union-list l1 l2)
    (cond [(null? l1) l2]
          [(null? l2) l1]
          [(= (car l1) (car l2))
           (cons (car l1)
                 (union-list (cdr l1) (cdr l2)))]
          [(> (car l1) (car l2))
           (cons (car l2)
                 (union-list l1 (cdr l2)))]
          [else (cons (car l1)
                      (union-list (cdr l1) l2))]))
  (cond [(null? set2) set1]
        [(null? set1) set2]
        [else (list->tree (union-list (tree->list set1)
                                 (tree->list set2)))]))


(test-case
 "Intersection set"
 (check-equal? (intersection-set '(4 (3 (1 () ())
                                        (2 () ()))
                                     (6 (5 () ())
                                        (7 () ())))
                                 '(7 (6 (5 () ())
                                        ())
                                     (9 (8 () ())
                                        (10 () ()))))
               '(6 (5 () ())
                   (7 () ()))))


(test-case
 "Union set"
 (check-equal? (union-set '(2 (1 () ()) (3 () ()))
                          '(5 (4 () ()) (6 () ())))
               '(3 (1 () (2 () ())) (5 (4 () ()) (6 () ())))))
