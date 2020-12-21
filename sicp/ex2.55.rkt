#lang racket
;ex2.55
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

(car ''abracadabra)
;   note:
;   ' is a shorthand for quote
;   so the above is equivalent to
(car (quote (quote abracadabra)))
;   and is equilavent to:
(car '(quote abracadabra))



