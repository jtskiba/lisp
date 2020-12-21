#lang racket
;ex2.27

; from ex2.18 - fn reverse
(define nil '()) 
  
(define (reverse items) 
  (define (iter items result) 
    (if (null? items) 
        result 
        (iter (cdr items) (cons (car items) result)))) 
  
  (iter items nil))

(define (reverse2 items) 
  (if (null? (cdr items)) 
      items 
      (append (reverse (cdr items)) 
              (cons (car items) nil)))) 
  
  
;now for ex2.27
; deep-reverse procedure that takes a list as argument
; and returns as its value the list with its elements reversed
; and with all sublists deep-reversed as well

;ok
(define x 
  (list (list 1 2) (list 3 4) (list 5 6)))

;not ok
(define y 
  (list (list (list 9 10) 2) (list 3 4) (list (list 5 6) (list 7 8))))

;not ok
(define z 
  (list 1 2 3 4 5))

(define (deep-reverse2 items) 
  (if (null? (cdr items)) 
      (if (pair? (car items))
          (deep-reverse2 (car items))
          items)
      (append (deep-reverse2 (cdr items)) 
              (cons (if (pair? (car items))
                        (deep-reverse2 (car items))
                        (car items))
                    nil)))) 


x
(reverse2 x)
(deep-reverse2 x)

z
(reverse2 z)
(deep-reverse2 z)

y
(reverse2 y)
(deep-reverse2 y)
;(deep-reverse3 y)
;(reverse3 y)

