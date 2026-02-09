#lang sicp

(#%require rackunit)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))


(test-case
 "(tree->list-1 tree)"
 (check-equal? (tree->list-1 '(6
                               (3 (2 (1
                                      () ()) ())
                                  (5 (4
                                      () ()) ()))
                               (9 (7 () (8
                                         () ()))
                                  (10 () (11
                                          () ())))))
               '(1 2 3 4 5 6 7 8 9 10 11)))


(define (tree->list-2 tree)
  (define (copy-to-list t result-list)
    (if (null? t)
        result-list
        (copy-to-list (left-branch t)
                      (cons (entry t)
                            (copy-to-list
                             (right-branch t)
                             result-list)))))
  (copy-to-list tree '()))


(test-case
 "(tree->list-2 tree)"
 (check-equal? (tree->list-2 '(6
                               (3 (2 (1
                                      () ()) ())
                                  (5 (4
                                      () ()) ()))
                               (9 (7 () (8
                                         () ()))
                                  (10 () (11
                                          () ())))))
               '(1 2 3 4 5 6 7 8 9 10 11)))