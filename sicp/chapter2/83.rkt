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
;--------------------------------------------------------------------------------


(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'make 'scheme-number (lambda (x) (tag x))))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ([g (gcd n d)])
      (cons (/ n g) (/ d g))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom))

(define (make-rational n d)
  ((get 'make 'rational) n d))


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

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y)))))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))


(define (scheme-number->rational num)
  (let ([n (contents num)])
    (make-rational (numerator n)
                   (denominator n))))
(put 'raise 'scheme-number scheme-number->rational)

(define (rational->complex num)
  (let* ([val (contents num)]
         [n ((get 'numer 'rational) val)]
         [d ((get 'denom 'rational) val)])
    (make-complex-from-real-imag (/ n d) 0.0)))
(put 'raise 'rational rational->complex)


(define (raise datum)
  (let* ([t (type-tag datum)]
         [t1->t2 (get 'raise t)])
    (if t1->t2
        (raise (t1->t2 datum))
        datum)))


(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-complex-package)

(check-equal? (raise 4.17)
              (make-complex-from-real-imag 4.17 0.0))
(check-equal? (raise (make-scheme-number 7))
              (make-complex-from-real-imag 7 0.0))
(check-equal? (raise (make-rational 3.0 4.0))
              (make-complex-from-real-imag 0.75 0.0))
