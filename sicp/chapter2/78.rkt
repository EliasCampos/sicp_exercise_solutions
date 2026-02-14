#lang sicp

(#%require rackunit)

;--------------------------------------------------------------------------------

(define (make-entry key value) (list key value))
(define (entry-key entry) (car entry))
(define (entry-value entry) (cadr entry))
(define (table-get table key)
  (define (get-iter t)
    (cond [(null? t)
           false]
          [(equal? key (entry-key (car t)))
           (entry-value (car t))]
          [else (get-iter (cdr t))]))
  (get-iter table))
(define (table-put table key value)
  (define (put-step t)
    (cond [(null? t)
           (list (make-entry key value))]
          [(equal? key (entry-key (car t)))
           (cons (make-entry key value)
                 (cdr t))]
          [else (cons (car t)
                      (put-step (cdr t)))]))
  (put-step table))


(define *op-table* '())

(define (get op type)
  (table-get *op-table* (list op type)))

(define (put op type item)
  (set! *op-table*
        (table-put *op-table* (list op type) item)))

;--------------------------------------------------------------------------------

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond [(pair? datum) (car datum)]
        [(number? datum) 'scheme-number]
        [else (error "Bad tagged datum: TYPE-TAG" datum)]))

(define (contents datum)
  (cond [(pair? datum) (cdr datum)]
        [(number? datum) datum]
        [else (error "Bad tagged datum: CONTENTS" datum)]))

(define (apply-generic operation . args)
  (let* ([type-tags (map type-tag args)]
         [proc (get operation type-tags)])
    (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
               (list operation type-tags)))))

;--------------------------------------------------------------------------------

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x))))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(install-scheme-number-package)

(define one (make-scheme-number 1))
(define two (make-scheme-number 2))
(define three (make-scheme-number 3))
(define four (make-scheme-number 4))

(test-case
 "The generic arithmetic operations should work with the scheme-number type."
 (check-equal? (add one two) three)
 (check-equal? (mul one two) two)
 (check-equal? (sub three two) one)
 (check-equal? (div four two) two))

(test-case
 "The generic arithmetic operations should work with primitive numbers as well."
 (check-eq? (add 2 2) 4)
 (check-eq? (add 3 4) 7)
 (check-eq? (sub 5 2) 3)
 (check-eq? (sub 2 7) -5)
 (check-eq? (mul 6 4) 24)
 (check-eq? (mul 5 0) 0)
 (check-equal? (mul 2.5 0.05) 0.125)
 (check-eq? (div 12 2) 6)
 (check-equal? (div 3 4) 3/4))
