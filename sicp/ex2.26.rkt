#lang racket
;ex2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

;what is printer for the following
(append x y)
;(1 2 3 4 5 6)
(cons x y)
;((1 2 3) 4 5 6)
(list x y)
;((1 2 3) (4 5 6))



