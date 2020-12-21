#lang racket
;ex2.17 - see below
(define one-through-four (list 1 2 3 4))
one-through-four

(car one-through-four)
(cdr one-through-four)
(cons 5 one-through-four)

(define odds (list 1 3 5 7))
(define squares (list 1 4 9 16 25))

(length odds)
(append squares odds)

; below - solution
(last-pair (list 23 72 149 34))

;my version
(define (last-pair2 list1)
  (if (null? (cdr list1))
      (list (car list1))
      (last-pair2 (cdr list1))))

(last-pair2 (list 23 72 149 34))



