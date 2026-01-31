#lang sicp

(#%require rackunit)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (let ([h (/ (- b a) n)])
    (define (y k)
      (* (f (+ a (* k h)))
         (cond [(or (= k 0) (= k n)) 1]
               [(even? k) 2]
               [else 4])))
    (* (/ h 3) (sum y 0 inc n))))


(define (identity x) x)
(define (cube x) (* x x x))


(define-binary-check (check-in-tolerance actual expected)
  (< (abs (- actual expected)) 0.001))


(test-case
  "Should calculate approximate value of integral of a function."
  (check-in-tolerance (simpson-integral (lambda (_) 1) 0 10.0 100) 10)
  (check-in-tolerance (simpson-integral identity 0 10.0 1000) 50.0)
  (check-in-tolerance (simpson-integral cube 0 1.0 1000) 0.25))
