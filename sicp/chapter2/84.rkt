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

(define (raise datum supertype)
  (let* ([t (type-tag datum)]
         [t1->t2 (get 'raise t)])
    (cond [(equal? (type-tag datum) supertype)
           datum]
          [t1->t2
           (raise (t1->t2 datum) supertype)]
          [else false])))

(define (apply-generic operation . arguments)
  (define (apply-procedure proc args)
    (apply proc (map contents args)))
  (define (coercion type-tags)
    (define (duplicate tag)
      (define (duplicate-iter acc items)
        (if (null? items)
            acc
            (duplicate-iter (cons tag acc) (cdr items))))
      (duplicate-iter '() type-tags))
    (define (all? items default)
      (cond [(null? items) default]
            [(not (car items)) false]
            [else (all? (cdr items) true)]))
    (define (try-coercion left right)
      (if (null? right)
          (error "No method for these types"
                 (list operation type-tags))
          (let* ([current-arg (car right)]
                 [current-type (type-tag current-arg)]
                 [rest (cdr right)]
                 [same-type-tags (duplicate current-type)]
                 [proc (get operation same-type-tags)]
                 [coerced (map (lambda (arg) (raise arg current-type)) arguments)])
            (if (and proc (all? coerced false))
                (apply-procedure proc coerced)
                (try-coercion (append left (list type-tag)) rest)))))
      (try-coercion '() arguments))

    (let* ([type-tags (map type-tag arguments)]
           [proc (get operation type-tags)])
      (if proc
          (apply-procedure proc arguments)
          (coercion type-tags))))

;--------------------------------------------------------------------------------

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (biggest a b c) (apply-generic 'biggest a b c))


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
  (put 'make 'scheme-number (lambda (x) (tag x)))

  (put 'biggest '(scheme-number scheme-number scheme-number)
       (lambda (a b c) (tag (max a b c)))))


(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (size x) (/ (numer x) (denom x)))
  
  (define (make-rat n d)
    (let ([g (gcd n d)])
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (bigger-than x y z)
    (and (> (size x) (size y))
         (> (size x) (size z))))
  (define (biggest-rat a b c)
    (cond [(bigger-than a b c) a]
          [(bigger-than b a c) b]
          [else c]))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'biggest '(rational rational rational)
       (lambda (a b c) (tag (biggest-rat a b c))))

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
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (compx-real z1) (compx-real z2))
                         (+ (compx-imag z1) (compx-imag z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (compx-real z1) (compx-real z2))
                         (- (compx-imag z1) (compx-imag z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (compx-magnitude z1) (compx-magnitude z2))
                       (+ (compx-angle z1) (compx-angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (compx-magnitude z1) (compx-magnitude z2))
                       (- (compx-angle z1) (compx-angle z2))))
  (define (bigger-than x y z)
    (and (> (compx-magnitude x)
            (compx-magnitude y))
         (> (compx-magnitude x)
            (compx-magnitude z))))
  (define (biggest-complex z1 z2 z3)
    (cond [(bigger-than z1 z2 z3) z1]
          [(bigger-than z2 z1 z3) z2]
          [else z3]))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
     (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
     (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'biggest '(complex complex complex)
       (lambda (a b c) (tag (biggest-complex a b c)))))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (compx-real z) (apply-generic 'compx-real z))
(define (compx-imag z) (apply-generic 'compx-imag z))
(define (compx-magnitude z) (apply-generic 'compx-magnitude z))
(define (compx-angle z) (apply-generic 'compx-angle z))


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


(install-scheme-number-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)


(test-case
 "Operation with multiple arguments must get a result as long as there are proper coercions."
 (check-equal? (biggest (make-complex-from-real-imag 2 3)
                        10
                        (make-rational 7 4))
               (make-complex-from-real-imag 10 0.0))
 (check-equal? (biggest (make-scheme-number 1.5)
                        0.5
                        (make-rational 5 4))
               (make-rational 3.0 2.0))
 (check-equal? (add 2 (make-complex-from-mag-ang 10 0))
               (make-complex-from-real-imag 12 0.0)))