#lang sicp

(#%require rackunit)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (filter op sequence)
  (cond [(null? sequence)
         nil]
        [(op (car sequence))
         (cons (car sequence) (filter op (cdr sequence)))]
        [else (filter op (cdr sequence))]))
(define (square n) (expt n 2))
(define (prime? n)
  (define (prime-check i)
    (cond [(> (square i) n) true]
          [(= (remainder n i) 0) false]
          [else (prime-check (+ i 1))]))
  (prime-check 2))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(check-equal? (prime-sum-pairs 6)
              '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11)))
