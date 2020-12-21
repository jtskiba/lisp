#lang racket
; from sicp
(define (square a) (* a a))
(define nil '())

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares 
                  (car tree))
                 (sum-odd-squares 
                  (cdr tree))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else
          (+ (fib (- n 2)) (fib (- n 1))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define item '(1 3 4 8))
(sum-odd-squares item)
(even-fibs 6)

;from sicp
;sequence operations

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))
(accumulate + 0 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(define (sum-odd-squares2 tree)
  (accumulate 
   +
   0
   (map square
        (filter odd?
                (enumerate-tree tree)))))

(define (even-fibs2 n)
  (accumulate 
   cons
   nil
   (filter even?
           (map fib
                (enumerate-interval 0 n)))))

(newline)
(define item1 '(1 3 4 8))
(sum-odd-squares  item1)
(sum-odd-squares2 item1)
(newline)
(even-fibs  6)
(even-fibs2 6)
(newline)

(define (list-fib-squares n)
  (accumulate 
   cons
   nil
   (map square
        (map fib
             (enumerate-interval 0 n)))))

(list-fib-squares 10)
(newline)

(define 
  (product-of-squares-of-odd-elements
   sequence)
  (accumulate 
   *
   1
   (map square (filter odd? sequence))))

(product-of-squares-of-odd-elements 
 (list 1 2 3 4 5))
(newline)

;ex2.33
(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) (map1 p (cdr sequence)))) 
              nil sequence))

(map  square '(1 2 3))
(map1 square '(1 2 3))
(newline)

(define (append1 seq1 seq2)
  (accumulate cons seq1 seq2))

(append item1 item1)
(append1 item1 item1)
(newline)

(define (length1 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

item1
(length item1)
(length1 item1)
