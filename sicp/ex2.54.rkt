#lang racket
;ex2.54
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;(equal? '(this is a list) 
;        '(this is a list))

;(equal? '(this is a list) 
;        '(this (is a) list))

;(eq? 'a 'a)

;ans

(define it1 '(this is a list))
(define it2 '(this (is a) list))


(define (equal?? item1 item2)
  (cond ((and (null? item1) (null? item2)) #t)
        ((not (eq? (car item1) (car item2))) #f)
        (else (equal?? (cdr item1) (cdr item2)))))

(equal? it1 it1)
(equal? it1 it2)
;(newline) ; replica below:
(equal?? it1 it1)
(equal?? it1 it2)
