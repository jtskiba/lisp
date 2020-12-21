#lang racket
;ex2.21
(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items)) 
            (square-list (cdr items)))))

(define (square-list2 items)
  (map * items items))

;(map + (list 1  2  3)
;       (list 10 20 30))

(square-list  (list 1 2 3 4))
(square-list2 (list 1 2 3 4))