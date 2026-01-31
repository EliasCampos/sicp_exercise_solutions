#lang sicp


(#%require rackunit)

(define (f-recur n)
  (cond [(< n 1) 0]
        [(< n 3) n]
        [else (+
               (f-recur (- n 1))
               (f-recur (- n 2))
               (f-recur (- n 3)))]))

(define (f-iter n)
  (define (f-iter-iter a b c count)
    (if (< count 3)
        a
        (f-iter-iter (+ a b c) a b (- count 1))))

  (cond [(< n 1) 0]
        [(< n 3) n]
        [else (f-iter-iter 2 1 0 n)]))

(test-case
  "Recursive function should return proper summ."
  (check-equal? (f-recur 0) 0)
  (check-equal? (f-recur 1) 1)
  (check-equal? (f-recur 2) 2)
  (check-equal? (f-recur 3) 3)
  (check-equal? (f-recur 4) 6)
  (check-equal? (f-recur 5) 11))

(test-case
  "Iterative function should return proper summ."
  (check-equal? (f-iter 0) 0)
  (check-equal? (f-iter 1) 1)
  (check-equal? (f-iter 2) 2)
  (check-equal? (f-iter 3) 3)
  (check-equal? (f-iter 4) 6)
  (check-equal? (f-iter 5) 11))