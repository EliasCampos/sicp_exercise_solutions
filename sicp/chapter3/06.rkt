#lang sicp

(#%require rackunit)

(define random-init 42)

(define (rand-update x) (inc x)) ; a dummy "random number generator"

(define rand (let ([x random-init])
               (lambda (m)
                 (cond [(eq? m 'generate)
                        (set! x (rand-update x))
                        x]
                       [(eq? m 'reset)
                        (lambda (number) (set! x number))]
                       [else (error "Invalid mode: RAND" m)]))))

(check-eq? (rand 'generate) 43)
(check-eq? (rand 'generate) 44)

((rand 'reset) 1)

(check-eq? (rand 'generate) 2)
(check-eq? (rand 'generate) 3)
