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

(define (=zero? x) (apply-generic '=zero? x))


(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))

  (put 'make 'scheme-number (lambda (x) (tag x)))

  (put '=zero? '(scheme-number) (lambda (x) (= x 0))))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(install-scheme-number-package)

(define zero (make-scheme-number 0))
(define one (make-scheme-number 1))
(define answer (make-scheme-number 42))

(test-case
 "(=zero? x) must work for primitive numbers."
 (check-true (=zero? zero))
 (check-false (=zero? one))
 (check-false (=zero? answer)))


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  
  (define (make-rat n d)
    (let ([g (gcd n d)])
      (cons (/ n g) (/ d g))))

  (define (zero-rational? x) (= (numer x) 0))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put '=zero? '(rational) zero-rational?))

(define (make-rational n d)
  ((get 'make 'rational) n d))


(install-rational-package)

(define rat-one-half (make-rational 1 2))
(define rat-one (make-rational 1 1))
(define rat-zero (make-rational 0 42))
(define rat-answer (make-rational 42 1))

(test-case
 "(=zero? x) must work for rational numbers."
 (check-false (=zero? rat-one-half))
 (check-false (=zero? rat-one))
 (check-true (=zero? rat-zero))
 (check-false (=zero? rat-answer)))


(define (square x) (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (compx-real z) (car z))
  (define (compx-imag z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (compx-magnitude z)
    (sqrt (+ (square (compx-real z))
             (square (compx-imag z)))))
  (define (compx-angle z)
    (atan (compx-imag z) (compx-real z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'compx-real '(rectangular) compx-real)
  (put 'compx-imag '(rectangular) compx-imag)
  (put 'compx-magnitude '(rectangular) compx-magnitude)
  (put 'compx-angle '(rectangular) compx-angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a)))))

(define (install-polar-package)
  ;; internal procedures
  (define (compx-magnitude z) (car z))
  (define (compx-angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (compx-real z) (* (compx-magnitude z) (cos (compx-angle z))))
  (define (compx-imag z) (* (compx-magnitude z) (sin (compx-angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'compx-real '(polar) compx-real)
  (put 'compx-imag '(polar) compx-imag)
  (put 'compx-magnitude '(polar) compx-magnitude)
  (put 'compx-angle '(polar) compx-angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
     (lambda (r a) (tag (make-from-mag-ang r a)))))


(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (compx-zero? z)
    (and (= (compx-real z) 0)
         (= (compx-imag z) 0)))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put '=zero? '(complex) compx-zero?))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (compx-real z) (apply-generic 'compx-real z))
(define (compx-imag z) (apply-generic 'compx-imag z))
(define (compx-magnitude z) (apply-generic 'compx-magnitude z))
(define (compx-angle z) (apply-generic 'compx-angle z))

(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define c01 (make-complex-from-real-imag 0 0))
(define c02 (make-complex-from-mag-ang 0 3.14))
(define c11 (make-complex-from-real-imag 1 1))
(define c21 (make-complex-from-real-imag 2 3))
(define c31 (make-complex-from-real-imag 2 -3))

(test-case
 "(=zero? x) must work for complex numbers."
 (check-true (=zero? c01))
 (check-true (=zero? c02))
 (check-false (=zero? c11))
 (check-false (=zero? c21))
 (check-false (=zero? c31)))
