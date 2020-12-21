#lang racket
;ex2.28
; procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the
; leaves of the tree arranged in left-to-right order.

(define x 
  (list (list 1 2 3) (list 4 5) (list 6 7)))

(define z1 
  (list (list 0) (list 1 2 3) (list 4 5) (list 6 7)))

(define z2 
  (list 0 (list 1 2 3) (list 4 5) (list 6 7)))

(define z3 
  (list (list 1 2 3) (list 4 5) (list 6 7) 8))

(define z4 
  (list (list 1 2 3) (list 4 5) 6 (list 7 8) 9))


(define y 
  (list x (list x z1 z2)))


(define (fringe items)
  (if (null? items)
      items
      (append (if (pair? (car items))
                  (fringe (car items))
                  (list (car items)))
              (if (pair? (cdr items))
                  (fringe (cdr items))
                  (cdr items)))))


z1
(fringe z1)
(newline)
z2
(fringe z2)
(newline)
z3
(fringe z3)
(newline)
z4
(fringe z4)
(newline)
y
(fringe y)
(fringe (fringe y))




