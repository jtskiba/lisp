#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment point)
  (car point))

(define (end-segment point)
  (cdr point))

(define (midpoint-segment segment)
  (cons (/ (+ (car (car segment)) (car (cdr segment))) 2)
        (/ (+ (cdr (car segment)) (cdr (cdr segment))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(x-point (make-point 2 3))
(y-point (make-point 2 4))

(define p1  (make-point 1 3))
(define p2  (make-point 13 -1))
(define seg (make-segment p1 p2))
(define midseg (midpoint-segment seg))

(print-point midseg)



