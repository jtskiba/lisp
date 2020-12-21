#lang racket
;ex2.42
;from previous two exercises below:
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

;from previus exercise
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
;(filter-sum-three 7 10)


;ex2.42
(define empty-board nil)
(define board-size 8)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (accumulate append nil (list rest-of-queens (list (list new-row k)))))

(define (safe? k positions)
  (all-true?
   (let ((row (car (list-ref positions (- k 1)))))
     (map
      (lambda (position)
        (if (or (= row (car position))
                (= row (+ (car position) (- k (cadr position))))
                (= row (- (car position) (- k (cadr position)))))
            #f
            #t))
      (reverse (cdr (reverse positions)))))))

;(list-ref '((3 1)(1 2)(4 3)) 2 )

(define (all-true? items)
  (if (null? items)
      #t
      (if (car items)
          (all-true? (cdr items))
          #f)))

;(all-true? '(#t #t #t #t #t #f #t #t))
  
;(safe? 4 '((3 1)(1 2)(4 3)(8 4)))
;(reverse (cdr (reverse '((3 1)(1 2)(4 3)))))
(queens board-size)




;(adjoin-position 4 3 '((3 1)(1 2)))




;(safe? 2 '((1 3)))  
