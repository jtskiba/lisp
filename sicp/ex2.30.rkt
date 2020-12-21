#lang racket
;from SICP: -
(define nil '())

(define (scale-tree1 tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) 
         (* tree factor))
        (else
         (cons (scale-tree1 (car tree) 
                           factor)
               (scale-tree1 (cdr tree) 
                           factor)))))

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor)))
       tree))

;(scale-tree1 (list 1 
;                  (list 2 (list 3 4) 5) 
;                  (list 6 7))
;            10)

;(scale-tree2 (list 1 
;                  (list 2 (list 3 4) 5) 
;                  (list 6 7))
;            10)


;ex2.30

(define (square a) (* a a))

(define (square-tree1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else
         (cons (square-tree1 (car tree))
               (square-tree1 (cdr tree))))))


(define (square-tree2 tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree2 subtree)
             (square subtree)))
         tree))


(square-tree1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
