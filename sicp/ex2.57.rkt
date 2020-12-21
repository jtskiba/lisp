#lang racket
;ex2.57

; sicp exxample re derivative and symbolics
;(symbol? 'a)
;(number? 2)

(define (variable? x) (symbol? x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend0 s)
  (if (> (length s) 3)
      (make-sum
            (addend (cdr s))
            (augend (cdr s)))
      (caddr s)))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))
  )
)  

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand0 p)
  (if (> (length p) 3)
      (make-product
            (multiplier (cdr p))
            (multiplicand (cdr p)))
      (caddr p)))


(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))
  )
)


(define (make-exponentiation m1 m2)
  (cond 
        ((=number? m2 1) m1)
        ((=number? m2 0) 1)
        ((and (number? m1) (number? m2)) 
         (expt m1 m2))
        (else (list '** m1 m2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))


(define (derivative exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (derivative (addend exp) var)
                   (derivative (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (derivative (multiplicand exp) var))
          (make-product 
           (derivative (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp) (- (exponent exp) 1))
           (derivative (base exp) var))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(derivative '(+ x 3 x 1 x x x (* x x)) 'x)
(derivative '(* x 3 x) 'x)
;(derivative '(** x 2) 'x)