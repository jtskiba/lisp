#lang sicp

;; ex1.41

(define (double fun)
  (lambda (x)
    (fun (fun x))))

(define (inc t) (+ 1 t))
(define (mul t) (* 2 t))

(inc 4)

((double inc) 5)

(mul 3)
((double mul) 3)
; ok - think this works
((double inc) 1)


;now for the puzzle... ?
(((double (double double)) inc) 5)




