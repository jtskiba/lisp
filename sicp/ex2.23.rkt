#lang racket
;ex2.23
(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))

;try to implement for-each

(define (for-each2 proc items)
  (if (null? items)
      (newline)
      (cons (proc (car items))
            (for-each2 proc (cdr items))
            )))

(for-each2 
 (lambda (x) (newline) (display x))
 (list 57 321 88))

;
(define (for-each3 proc items)
  (if (null? items)
      null
      (let ()
        (proc (car items))
        (for-each proc (cdr items)))))


(for-each3 
 (lambda (x) (newline) (display x))
 (list 57 321 88))