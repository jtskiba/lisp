#lang racket
;ex2.22 - why reversed?
(define (square a) (* a a))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(square-list  (list 1 2 3 4))
;this is because in cons, the square is first and then answer - but swapping gives funny results as shown below:

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items null))

(square-list2  (list 1 2 3 4))

;this is due to
(cons null (list 1))
(cons (list 1) null)