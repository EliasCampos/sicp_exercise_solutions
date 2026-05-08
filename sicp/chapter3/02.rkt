#lang sicp

(#%require rackunit)

(define (make-monitored f)
  (let ([counter 0])
    (lambda (x)
      (cond [(eq? x 'how-many-calls?)
             counter]
            [(eq? x 'reset-count)
             (set! counter 0)]
            [else
             (begin
               (set! counter (inc counter))
               (f x))]))))


(define s (make-monitored sqrt))


(define PRECISION 0.1)
(define-binary-check (check-in-tolerance actual expected)
  (<= (abs (- actual expected)) (* expected PRECISION)))

(check-in-tolerance (s 100) 10)
(check-eq? (s 'how-many-calls?) 1)
(check-in-tolerance (s 4) 2)
(check-eq? (s 'how-many-calls?) 2)
(s 'reset-count)
(check-eq? (s 'how-many-calls?) 0)