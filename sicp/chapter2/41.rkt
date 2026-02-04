#lang sicp

(#%require rackunit)

(define (filter op sequence)
  (cond [(null? sequence)
         nil]
        [(op (car sequence))
         (cons (car sequence) (filter op (cdr sequence)))]
        [else (filter op (cdr sequence))]))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))

(define (triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map
         (lambda (k) (list i j k))
         (enumerate-interval 1 n)))
      (enumerate-interval 1 n)))
   (enumerate-interval 1 n)))
(define (unique-triples n)
  (filter
   (lambda (triple)
     (let ([i (car triple)]
           [j (cadr triple)]
           [k (caddr triple)])
       (and (not (= i j))
            (not (= j k))
            (not (= i k)))))
   (triples n)))
(define (unique-triples-for-sum n s)
  (filter
   (lambda (triple)
     (let ([i (car triple)]
           [j (cadr triple)]
           [k (caddr triple)])
       (= (+ i j k) s)))
   (unique-triples n)))

(define x (unique-triples-for-sum 5 8))
(check-equal? (accumulate + 0 (car x)) 8)
(check-equal? (accumulate + 0 (cadr x)) 8)
(check-equal? (accumulate + 0 (caddr x)) 8)
(check-equal? (accumulate + 0 (cadddr x)) 8)