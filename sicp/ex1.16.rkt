#lang sicp

;standard way to do exponentiation
;using recursive process
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;using iterative process (less intuitive)
(define (expt2 b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))


;fast way to do exponentiation
;using recurssive process
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

;using iterative process (ex 1.16) - my version but not properly iterative
(define (fast-expt2 b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (fast-expt-iter b (/ counter 2) (* product (fast-expt-iter b (/ counter 2) 1))))
        (else (fast-expt-iter b (- counter 1) (* b product)))))

;using iterative process (ex 1.16) - from internet solutions (agreed!) as 3^6 is the same as (3^2)^(6/2)
(define (fast-expt3 b n)
  (fast-expt-iter3 b n 1))
(define (fast-expt-iter3 b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (fast-expt-iter3 (square b) (/ counter 2) product))
        (else (fast-expt-iter b (- counter 1) (* b product)))))

(fast-expt 11 10)
(fast-expt2 11 10)
(fast-expt3 11 10)

(fast-expt 30 7)
(fast-expt2 30 7)
(fast-expt3 30 7)