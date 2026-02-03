#lang sicp

(#%require rackunit)

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(check-equal? (fold-right / 1 (list 1 2 3)) (/ 3 2))
(check-equal? (fold-left / 1 (list 1 2 3)) (/ 1 6))
(check-equal? (fold-right list nil (list 1 2 3)) '(1 (2 (3 ()))))
(check-equal? (fold-left list nil (list 1 2 3)) '(((() 1) 2) 3))

(define s (list 1 2 3 4 5 6))
(define s1 (list 1 2 3))
(define s2 (list 4 5 6))
(define s3 (list 7 8 9))
(test-case
  "To have fold-right and fold-left produce the same result, (op a b) must be equal (op b a)."
  (check-equal? (fold-right + 0 s) (fold-left + 0 s))
  (check-equal? (fold-right * 1 s) (fold-left * 1 s))
  (check-equal? (fold-right append nil (list s1 s2 s3))
                (fold-left append nil (list s1 s2 s3))))

