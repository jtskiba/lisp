#lang racket
;ex2.60
; now working with possible duplicates within set (ok)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 (remove-from-set (car set1) set2))))
        (else (intersection-set (cdr set1) 
                                set2))))

(define (remove-from-set x set) ; removes only one instance of x, not all from the set
  (cond ((null? set) '())
        ((not (equal? x (car set))) (cons (car set) (remove-from-set x (cdr set))))
        (else (cdr set))))

(define (union-set set1 set2)
  (append set1 set2))

(element-of-set? 2 '(2 3 2 1 3 2 2)) ;no change to element-of-set?
(adjoin-set 2 '(2 3 2 1 3 2 2)) ; removed condition checking if x in set, we add it anyway even if duplicate
(intersection-set '(2 2 3 3 3) '(2 3 2 1 3 2 2)) ;i want (2 2 3 3) - done - had to create remove-from-set where single instance of x is removed from set
(remove-from-set 2 '(1 2 2 2 3)); example of how this new fn works
(union-set '(2 2 3 3 3) '(2 3 2 1 3 2 2)) ; this will now be effectively an append fn
