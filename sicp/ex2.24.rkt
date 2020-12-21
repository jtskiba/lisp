#lang racket
;ex2.24
;first from book
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))

(length x)
;3

(count-leaves x)
;4

(list x x)
;(((1 2) 3 4) ((1 2) 3 4))

(length (list x x))
;2

(count-leaves (list x x))
;8


;now the exercise
(list 1 (list 2 (list 3 4)))
;done on paper