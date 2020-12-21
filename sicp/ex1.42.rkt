#lang sicp

;; ex1.42
(define (inc t) (+ 1 t))
(define (square t) (* t t))

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)





