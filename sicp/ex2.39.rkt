#lang racket
;ex2.39
(define nil '())
;accumulate is also called fold-right because it combines the first element of
;the sequence with the result of combining all the elements to the right
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op 
                      initial 
                      (cdr sequence)))))

;fold-left:
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;task:
(define (reverseR sequence)
  (fold-right 
   (lambda (x y) (if (null? y)
                     (list (append y x))
                     (append y (list x))))
     nil sequence))


(define (reverseL sequence)
  (fold-left 
   (lambda (x y)
     (append (if (null? y)
                 y
                 (list y))
             x))
   nil sequence))


(define x '(1 2 3 4))
(reverseR x)
(reverseL x)