#lang sicp

(#%require rackunit)

(define (same-parity num . nums)
  (define (same-parity? x y) (= (remainder x 2) (remainder y 2)))
  (define (filtered-nums curr-nums)
   (cond [(null? curr-nums) nil]
         [(same-parity? num (car curr-nums))
          (cons (car curr-nums) (filtered-nums (cdr curr-nums)))]
         [else (filtered-nums (cdr curr-nums))]))
  (cons num (filtered-nums nums)))

(test-case
  "Should return list of arguments with same parity as in the first number."
  (check-equal? (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))
  (check-equal? (same-parity 2 3 4 5 6 7) (list 2 4 6))
  (check-equal? (same-parity 10 5 7 11 13) (list 10))
  (check-equal? (same-parity 9 3 6 5 12) (list 9 3 5))
  (check-equal? (same-parity 17) (list 17)))