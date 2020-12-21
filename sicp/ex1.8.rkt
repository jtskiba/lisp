#lang sicp

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x)
                 x)))

(define (improve guess x)
  (by-three (/ x (* guess guess)) (* 2.0 guess)))

(define (by-three x y)
  (/ (+ x y) 3))

(define (good-enough? guess x)
  (< (abs (/ (- (improve guess x) guess) guess )) 0.0001))

(define (cube x)
  (cube-iter 1.0 x))


(cube 0.000001) ; results in 0.01
(cube 20000000000000000) ; 271441.7616594
