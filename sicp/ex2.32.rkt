#lang racket
;ex2.32
(define nil '())

(define (fringe items)
  (if (null? items)
      items
      (append (if (pair? (car items))
                  (fringe (car items))
                  (list (car items)))
              (if (pair? (cdr items))
                  (fringe (cdr items))
                  (cdr items)))))
; mine - 
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map
                 (lambda (x) (fringe (list (cons (car s) x))))
                 rest)))))

;solution (just removed fringe and list)
(define (subsets2 s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets2 (cdr s))))
        (append rest
                (map
                 (lambda (x) (cons (car s) x))
                 rest)))))

(define x '(1 2 3))

(subsets x)

