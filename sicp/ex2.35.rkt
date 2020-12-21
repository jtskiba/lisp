#lang racket

;ex2.35
(define nil '())

(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (count-leaves0 x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves0 (car x))
                 (count-leaves0 (cdr x))))))

(define (count-leaves1 t)
  (accumulate (lambda(a b) (+ 1 b)) 0 (fringe t)))

(define (count-leaves t)
  (accumulate (lambda(a b) (+ (if (pair? a)
                                    (count-leaves a)
                                    1)
                              b))
              0
              (map append t)))


(define x (cons (list 1 2) (list 3 4)))
(count-leaves0 x)
(count-leaves0 (list x x))
(newline)
(count-leaves1 x)
(count-leaves1 (list x x))
(newline)
(count-leaves x)
(count-leaves (list x x))

