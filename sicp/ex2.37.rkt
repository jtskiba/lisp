#lang racket

;ex2.37
(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda(x) (list-ref x 0)) seqs))
            (accumulate-n op init (map (lambda(x) (cdr x)) seqs)))))

;define vars
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define n (list (list 1 2 1) (list 0 2 1) (list 1 3 2) (list 4 0 1)))
(define v (list 0 1 3 5))
(define w (list 1 2 2 1))

;(dot-product v w)
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;(matrix-*-vector m v)
(define (matrix-*-vector m v)
  (map (lambda(row)(dot-product row v)) m))

;(matrix-*-vector m v)

;(transpose m)
(define (transpose mat)
  (accumulate-n cons nil mat))

;(matrix-*-matrix m n) 
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(row)(matrix-*-vector cols row)) m)))

(matrix-*-matrix m n)


