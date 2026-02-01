#lang sicp

(#%require rackunit)

(define (my-for-each proc values)
  (define (step vals)
    (proc (car vals))
    (for-iter (cdr vals)))
  (define (for-iter vals)
    (if (null? vals)
      true
      (step vals)))
  (for-iter values))

(check-true (my-for-each (lambda (x)
               (newline)
               (display x))
             (list 57 321 88)))
