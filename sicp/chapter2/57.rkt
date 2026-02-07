#lang sicp

(#%require rackunit)

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [(sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))]
        [(exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv (base exp) var))]
        [else
         (error "unknown expression type: DERIV" exp)]))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (shrink-numbers proc items)
  (define (combine curr rest)
    (let ([rest-val (car rest)]
          [rest-items (cdr rest)])
      (cond [(and (number? curr) (number? rest-val))
             (cons (proc curr rest-val) rest-items)]
            [(number? rest-val)
             (cons rest-val (cons curr rest-items))]
            [else
             (cons curr rest)])))
  (define (walk l)
    (if (or (null? l)
            (null? (cdr l)))
        l
        (combine (car l) (walk (cdr l)))))
  (walk items))

(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [(or (sum? a1) (sum? a2))
         (cons '+
               (shrink-numbers +
                               (append (if (sum? a1) (cdr a1) (list a1))
                                       (if (sum? a2) (cdr a2) (list a2)))))]                     
        [else (list '+ a1 a2)]))
(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [(or (product? m1) (product? m2))
         (cons '*
               (shrink-numbers *
                               (append (if (product? m1) (cdr m1) (list m1))
                                       (if (product? m2) (cdr m2) (list m2)))))]  
        [else (list '* m1 m2)]))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
      
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (make-exponentiation base exponent)
  (cond [(= exponent 0) 1]
        [(number? base) (expt base exponent)]
        [(= exponent 1) base]
        [else (list '** base exponent)]))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))


(test-case
 "Addition is simplified properly."
 (check-equal? (make-sum 1 '(+ 2 a))
               '(+ 3 a))
 (check-equal? (make-sum 1 '(+ a 3))
               '(+ 4 a))
 (check-equal? (deriv '(+ a x 1) 'x)
               1)
 (check-equal? (deriv '(+ (* a x) b) 'x)
               'a)
 (check-equal? (deriv '(+ (* a (** x 2)) (* 2 a b x) (* b (** x 2))) 'x)
               '(+ (* 2 a x) (* 2 a b) (* 2 b x)))
 (check-equal? (deriv '(+ (* 2 x) (* 3 x) (* 4 x)) 'x)
               9)
 (check-equal? (deriv '(+ (* 2 (** x 2)) (* 3 (** x 2)) (* 4 (** x 2))) 'x)
               '(+ (* 4 x) (* 6 x) (* 8 x))))

(test-case
 "Multiplication is simplified properly."
 (check-equal? (deriv '(* a x) 'x)
               'a)
 (check-equal? (deriv '(* (** a 2) (** b 2) (** x 2)) 'x)
               '(* 2 (** a 2) (** b 2) x))
 (check-equal? (deriv '(* (** (+ a x) 2) (** (+ b x) 2)) 'x)
               '(+ (* 2 (** (+ a x) 2) (+ b x)) (* 2 (+ a x) (** (+ b x) 2))))
 (check-equal? (deriv '(* 2 (** x 2)) 'x)
               '(* 4 x)))