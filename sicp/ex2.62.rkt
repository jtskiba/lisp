#lang racket
;ex2.62 - ordered sets
;from sicp
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (adjoin-set2 x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set2 x (cdr set))))))

(define (remove-from-set x set) ; removes only one instance of x, not all from the set
  (cond ((null? set) '())
        ((not (equal? x (car set))) (cons (car set) (remove-from-set x (cdr set))))
        (else (cdr set))))


(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        ((and (null? set1) (null? set2)) '())
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
              (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

(define (union-set2 set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        ((and (null? set1) (null? set2)) '())
        ((< (car set1) (car set2))
         (cons (car set1) (union-set2 (cdr set1) set2)))
        ((= (car set1) (car set2))
         (cons (car set1) (union-set2 (cdr set1) (cdr set2))))
        (else (cons (car set2) (union-set2 set1 (cdr set2))))))


;ex2.62

(union-set '(1 2 3) '(3 4 5))
(union-set2 '(1 2 3) '(3 4 5))
(union-set2 '(1 2 7 8) '(3 4 5))
(union-set2 '(1 4 9) '(3 5 7))


