#lang sicp


(#%require rackunit)

(define (apply-generic op arg) (arg op))

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond [(eq? op 'real-part) (* r (cos a))]
          [(eq? op 'imag-part) (* r (sin a))]
          [(eq? op 'magnitude) r]
          [(eq? op 'angle) a]
          [else (error "Unknown op: MAKE-FROM-REAL-IMAG" op)]))
  dispatch)


(define cn (make-from-mag-ang 1.41421356237 0.78539816339))


(define PRECISION 0.001)
(define-binary-check (check-in-tolerance actual expected)
  (<= (abs (- actual expected)) (* expected PRECISION)))

(check-in-tolerance (apply-generic 'real-part cn) 1)
(check-in-tolerance (apply-generic 'imag-part cn) 1)
(check-equal? (apply-generic 'magnitude cn) 1.41421356237)
(check-equal? (apply-generic 'angle cn) 0.78539816339)