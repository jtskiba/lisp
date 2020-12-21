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

;(car2 (cons2 3 5))



(define (cdr2 z) 
  (z (lambda (p q) q)))
;(cdr2 (cons2 3 5))

;(define (moja x y) (+ x y))

;(moja 2 3)
;((lambda (x y) (+ x y)) 2 3)

;ex2.5
(define (cons3 x y)
  (* (expt 2 x) (expt 3 y)))

;(cons3 4 2)

(define (car3 number)
  (let ((a 2) (b 3) (count 0))
    (define (car-iter number count)
      (if (= (gcd number b) b)
          (car3 (/ number b))
          (if (= (gcd number a) a)
              (car-iter (/ number a) (+ count 1))
              count)))
    (car-iter number count)))

  
(define (cdr3 number)
  (let ((a 2) (b 3) (count 0))
    (define (cdr-iter number count)
      (if (= (gcd number a) a)
          (cdr3 (/ number a))
          (if (= (gcd number b) b)
              (cdr-iter (/ number b) (+ count 1))
              count)))
    (cdr-iter number count)))

;(car3 144)
;(cdr3 144)

(car3 (cons3 6 3))
(cdr3 (cons3 6 3))






;(car3 (cons3 3 5))




