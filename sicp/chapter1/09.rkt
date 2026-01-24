#lang sicp

(define (add-recur a b)
  (if (= a 0)
      b
      (inc (add-recur (dec a) b))))
(add2 4 10)

(define (add-iter a b)
  (if (= a 0)
      b
      (add-iter (dec a) (inc b))))
(add2 4 10)