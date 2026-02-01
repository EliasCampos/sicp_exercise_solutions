#lang sicp


(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval
     (min p1 p2 p3 p4)
     (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ([one (make-interval 1 1)])
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define R1 (make-interval 6.12 7.48))
(define R2 (make-interval 4.465 4.936))

(par1 R1 R2)
(par2 R1 R2)

(define A (make-interval 7.992 8.008))
(define B (make-interval 3.996 4.004))

(div-interval A A)
(div-interval A B)