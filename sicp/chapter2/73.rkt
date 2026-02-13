#lang sicp

(#%require rackunit)

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


(define (deriv expr var)
  (cond [(number? expr) 0]
        [(variable? expr) (if (same-variable? expr var) 1 0)]
        [else
         ((get 'deriv (operator expr)) (operands expr)
                                       var)]))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (install-deriv-package)
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond [(=number? a1 0) a2]
          [(=number? a2 0) a1]
          [(and (number? a1) (number? a2))
           (+ a1 a2)]
          [else (list '+ a1 a2)]))
  (define (make-product m1 m2)
    (cond [(or (=number? m1 0) (=number? m2 0)) 0]
          [(=number? m1 1) m2]
          [(=number? m2 1) m1]
          [(and (number? m1) (number? m2)) (* m1 m2)]
          [else (list '* m1 m2)]))

  (define (deriv-sum expr var)
    (define (addend s) (car s))
    (define (augend s) (cadr s))
    (make-sum (deriv (addend expr) var)
              (deriv (augend expr) var)))
  (define (deriv-product expr var)
    (define (multiplier p) (car p))
    (define (multiplicand p) (cadr p))
    (make-sum
          (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
          (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product))


(install-deriv-package)

(check-equal? (deriv '(+ (* a x) b) 'x) 'a)
(check-equal? (deriv '(* k (+ x a)) 'x) 'k)
