#lang sicp
;example of procedural implementation of pairs from the SICP
(define (cons1 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else 
           (error "Argument not 0 or 1:
                   CONS" m))))
  dispatch)

(define (car1 z) (z 0))
(define (cdr1 z) (z 1))

;(car1 (cons1 3 5))
;(cdr1 (cons1 3 5))

;ex2.4
;alternative procedural representation of pairs
(define (cons2 x y) 
  (lambda (m) (m x y)))

(define (car2 z) 
  (z (lambda (p q) p)))

(car2 (cons2 3 5))



(define (cdr2 z) 
  (z (lambda (p q) q)))
(cdr2 (cons2 3 5))

(define (moja x y) (+ x y))

(moja 2 3)
((lambda (x y) (+ x y)) 2 3)


