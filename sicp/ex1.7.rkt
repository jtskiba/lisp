#lang sicp

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt-iterV2 guess x)
  (if (good-enoughV2? guess x)
      guess
      (sqrt-iterV2 (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enoughV2? guess x)
  (or (< (abs (- (   square guess   )     x)) 0.001)
      (< (abs (/ (- (improve guess x) guess) guess )) 0.0001)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square-root x)
  (sqrt-iterV2 1.0 x))

(define (square  x)
  (* x x))

(square-root 11987655494443)
`(sqrt 11987655494443) 'thiswontend


;My better alternative (inspire below)



(define (sqrt-iterV2 guess x)
  (if (good-enoughV2? guess x)
      guess
      (sqrt-iterV2 (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enoughV2? guess x)
  (< (abs (/ (- (improve guess x) guess) guess )) 0.0001))

(define (sqrt x)
  (sqrt-iterV2 1.0 x))


(sqrt 0.000001) ; results in 0.0010000001533016628

(sqrt 20000000000000000) ; 141421428.14615405




;From solutions:

(define (square x)
    (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess previous)
  (> (/ (min guess previous) (max guess previous)) 0.99))

(define (sqrt-iter guess previous x)
  (if (good-enough? guess previous)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x x))

(sqrt 0.000001) ; results in 0.0010000001533016628

(sqrt 20000000000000000) ; 141421428.14615405