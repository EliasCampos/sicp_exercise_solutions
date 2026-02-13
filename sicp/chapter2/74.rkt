#lang sicp

(#%require rackunit)

;---------------------------------------------------
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

;---------------------------------------------------
(define file-of-d1 '(("Ann" "A4" 3000)
                     ("Bob" "B2" 4200)
                     ("Carl" "C3" 5100)))
(define file-of-d2 '(((name "Ann") (address "A4") (salary 3000))
                     ((address "B2") (name "Bob") (salary 4200))
                     ((salary 5100) (name "Carl") (address "C3"))))


(define (make-division-item division datum)
  (cons division datum))
(define (get-item-division item)
  (car item))
(define (get-division-datum item)
  (cdr item))

(define (get-record file name)
  (let* ([division (get-item-division file)]
         [op (get 'get-record division)]
         [division-file (get-division-datum file)]
         [record (op division-file name)])
    (make-division-item division record)))


(define (install-division-d1)
  (define (get-d1-record file name)
    (cond [(null? file)
            '()]
          [(equal? (caar file) name)
           (car file)]
          [else (get-d1-record (cdr file) name)]))
  (define (get-d1-salary record)
    (caddr record))
  
  (put 'get-record 'D1 get-d1-record)
  (put 'get-salary 'D1 get-d1-salary))

(install-division-d1)
(define d1-file (make-division-item 'D1 file-of-d1))
(test-case
 "(get-record file name) for division D1"
 (check-equal? (get-record d1-file "Bob") '(D1 "Bob" "B2" 4200))
 (check-equal? (get-record d1-file "Ann") '(D1 "Ann" "A4" 3000))
 (check-equal? (get-record d1-file "Carl") '(D1 "Carl" "C3" 5100))
 (check-equal? (get-record d1-file "Nope") '(D1)))


(define (install-division-d2)
  (define (get-val key record)
    (if (equal? (caar record) key)
        (cadar record)
        (get-val key (cdr record))))
  (define (get-d2-record file name)
    (cond [(null? file)
            '()]
          [(equal? (get-val 'name (car file)) name)
           (car file)]
          [else (get-d2-record (cdr file) name)]))
  (define (get-d2-salary record) (get-val 'salary record))
  
  (put 'get-record 'D2 get-d2-record)
  (put 'get-salary 'D2 get-d2-salary))

(install-division-d2)
(define d2-file (make-division-item 'D2 file-of-d2))
(test-case
 "(get-record file name) for division D2"
 (check-equal? (get-record d2-file "Bob")'(D2 (address "B2") (name "Bob") (salary 4200)))
 (check-equal? (get-record d2-file "Ann") '(D2 (name "Ann") (address "A4") (salary 3000)))
 (check-equal? (get-record d2-file "Carl") '(D2 (salary 5100) (name "Carl") (address "C3")))
 (check-equal? (get-record d2-file "Nope") '(D2)))

;---------------------------------------------------
(define (get-salary record)
  (let* ([division (get-item-division record)]
         [op (get 'get-salary division)]
         [division-record (get-division-datum record)])
    (op division-record)))

(install-division-d1)
(test-case
 "(get-record file name) for division D1"
 (check-eq? (get-salary (make-division-item 'D1 '("Ann" "A4" 3000))) 3000)
 (check-eq? (get-salary (make-division-item 'D1 '("Bob" "B2" 4200))) 4200)
 (check-eq? (get-salary (make-division-item 'D1 '("Carl" "C3" 5100))) 5100))

(install-division-d2)
(test-case
 "(get-record file name) for division D1"
 (check-eq? (get-salary (make-division-item 'D2 '((address "B2") (name "Bob") (salary 4200))))
            4200)
 (check-eq? (get-salary (make-division-item 'D2 '((name "Ann") (address "A4") (salary 3000))))
            3000)
 (check-eq? (get-salary (make-division-item 'D2 '((salary 5100) (name "Carl") (address "C3"))))
            5100))