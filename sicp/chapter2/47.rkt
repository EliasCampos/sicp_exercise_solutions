#lang sicp

(#%require rackunit)

(define (make-vect x y)
  (list x y))

(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame-1 frame)
  (car frame))
(define (edge1-frame-1 frame)
  (cadr frame))
(define (edge2-frame-1 frame)
  (caddr frame))

(define v1 (make-vect 0 0))
(define v2 (make-vect 1 1))
(define v3 (make-vect -1 1))

(define fr1 (make-frame-1 v1 v2 v3))
(check-equal? (origin-frame-1 fr1) v1)
(check-equal? (edge1-frame-1 fr1) v2)
(check-equal? (edge2-frame-1 fr1) v3)

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2 frame)
  (car frame))
(define (edge1-frame-2 frame)
  (cadr frame))
(define (edge2-frame-2 frame)
  (cddr frame))

(define fr2 (make-frame-2 v1 v2 v3))
(check-equal? (origin-frame-2 fr2) v1)
(check-equal? (edge1-frame-2 fr2) v2)
(check-equal? (edge2-frame-2 fr2) v3)
