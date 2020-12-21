#lang racket
;ex2.41
;from previous exercise below:
(define nil '())

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
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (square a) (* a a))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))

; creating pairs of numbers from 1 to n in each place
(define n 6)

;(accumulate 
; append
; nil
; (map (lambda (i)
;        (map (lambda (j) 
;               (list i j))
;             (enumerate-interval 1 (- i 1))))
;      (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j) 
                  (list i j))
                (enumerate-interval 
                 1 
                 (- i 1))))
         (enumerate-interval 1 n)))))

;(prime-sum-pairs n)

;now another example for SICP - list of all permutations
(define (remove1 item sequence) ; remove all items in item except seq
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)   ; empty set?
      (list nil)  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) 
                        (cons x p))
                      (permutations 
                       (remove1 x s)))) ;remove from library works oks
               s)))

;(remove1 2 '(1 2 3 4 5)) ; remove all items except item (2 3)
;(permutations '(1 2 3))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) 
            (list i j))
          (enumerate-interval 
           1 
           (- i 1))))
   (enumerate-interval 1 n)))

;ex2.41
(define (triplet n)
  (flatmap
   (lambda(z)
     (flatmap
      (lambda(y)
        (map (lambda(x)
               (list z y x))
             (enumerate-interval 1 (- y 1))))
      (enumerate-interval 1 (- z 1))))
   (enumerate-interval 1 n)))

(define (sum-of-three item)
  (+ (car item) (cadr item) (cadr (cdr item))))

(define (make-triple-sum item)
  (append item (list (sum-of-three item))))

(define (triple-sum? item s)
  (= (sum-of-three item) s))

(define (filter-sum-three n s)
  (map (lambda (x) (make-triple-sum x))
       (filter (lambda (item) (triple-sum? item s))
               (triplet n))))

;(unique-pairs 4)
;(flatmap (lambda(z) (flatmap (lambda(y) (map (lambda(x) (list z y x)) (list 1 2 3))) (list 1 2 3))) (list 1 2 3))
;(map (lambda(y) (map (lambda(x) (list y x)) (list 1 2 3))) (list 1 2 3))
;(triplet 5)
;(sum-of-three (list 3 5 7))
;(make-triple-sum (list 3 5 7))
;(triple-sum? (list 3 5 7) 15)

(filter-sum-three 7 10)
