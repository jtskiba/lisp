#lang racket
;ex2.38
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

;let's create fold-left:
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;test values
(fold-right + 0 (list 1 2 3))
(fold-left  + 0 (list 1 2 3))

(fold-right / 1 (list 1 2 3))
(fold-left  / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left  list nil (list 1 2 3))



