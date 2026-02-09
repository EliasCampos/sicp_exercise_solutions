#lang sicp

(#%require rackunit)


(define (make-tree entry left right)
  (list entry left right))

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


(check-equal?
 (list->tree '(1 3 5 7 9 11))
 '(5 (1 () (3 () ()))
     (9 (7 () ()) (11 () ()))))
 