#lang racket

(define-syntax cons-stream ;this is from (*) and key to making this work
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define the-empty-stream '())

(define (stream-null? stream) (null? stream))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream 
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-map2 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map2 
               (cons proc (map stream-cdr argstreams))))))

(define (square a) (* a a))

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))


(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))




(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Inifnite streams
'(Inifnite streams)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;integers

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) 
                   (not (divisible? x 7)))
                 integers))

;no-sevens

;(stream-ref no-sevens 100) ; 117 ; to access certain elements from the stream

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

;fibs
;(stream-ref fibs 10)

;sieve of Eratosthenes
'(sieve of Eratosthenes)

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? 
                   x (stream-car stream))))
           (stream-cdr stream)))))

(define primes 
  (sieve (integers-starting-from 2)))

;primes

;(stream-for-each display-line primes) this will start producing primes and will eventually crash memory

;(stream-ref primes 50) ; 233

; Application of sieve to Henderson diagram


; Defining streams implicitly

(define ones (cons-stream 1 ones))
;ones
 
(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define integers2 
  (cons-stream 1 (add-streams ones integers2)))

;(stream-ref integers 10)

(define fibs2 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
         (stream-cdr fibs2) fibs2))))

;(stream-ref fibs 3)

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define double 
  (cons-stream 1 (scale-stream double 2))) ; 1 2 4 8 16 32 64

;; prime definitions revisited

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (null? n)
      #f
      (= n (smallest-divisor n))))

;; contructing primes using implicit stream definitions

(define primes2
  (cons-stream
   2 (stream-filter 
      prime? (integers-starting-from 3))))

(define primes3 (stream-filter prime? (integers-starting-from 2)))


; agree - here it makes no differences.... unless we define a different predicate primes?
; this ones we chech by dividing only by primes, not by all numbers less than sqaure root of n
; thats why we have this initial cons-stream 2 ....
(define primes4
  (cons-stream
   2 (stream-filter 
      prime2? (integers-starting-from 3))))

(define (prime2? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes4))


;(stream-ref primes2 100000) ; 10 sec
;(stream-ref primes3 100000) ; 10 sec
;(stream-ref primes4 100000) ; 4 sec

;(stream-ref primes2 300000) ; 50 sec
;(stream-ref primes3 300000) ; 50 sec
;(stream-ref primes4 300000) ; 19 sec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.53
'(ex 3.53)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-streams2 s1 s2) 
  (stream-map2 + s1 s2))


;(define s (cons-stream 1 (add-streams2 s s)))
;(list (stream-ref s 0) (stream-ref s 1) (stream-ref s 2) (stream-ref s 3) (stream-ref s 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.54
'(ex 3.54)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mul-streams s1 s2) 
  (stream-map2 * s1 s2))

(define factorials 
  (cons-stream 1 (mul-streams factorials integers)))

;(list (stream-ref factorials 0) (stream-ref factorials 1) (stream-ref factorials 2) (stream-ref factorials 3) (stream-ref factorials 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.55
'(ex 3.55)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define partial-sum 
  (cons-stream 0 (add-streams2 partial-sum integers)))

;(list (stream-ref partial-sum 0) (stream-ref partial-sum 1) (stream-ref partial-sum 2) (stream-ref partial-sum 3) (stream-ref partial-sum 4))

; 0 1 2 3 4 5 6 7
; partial sums 1 3 6 10 15 21

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.56
'(ex 3.56)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define factor-2 
  (cons-stream 1 (scale-stream factor-2 2)))

(define factor-3 
  (cons-stream 1 (scale-stream factor-3 3)))

(define factor-5 
  (cons-stream 1 (scale-stream factor-5 5)))

;(list (stream-ref factor-3 0) (stream-ref factor-3 1) (stream-ref factor-3 2) (stream-ref factor-3 3) (stream-ref factor-3 4))

;merge two streams and remove repetitions
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (merge (stream-cdr s1) 
                          s2)))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (merge s1 
                          (stream-cdr s2))))
                 (else
                  (cons-stream 
                   s1car
                   (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (stream-cdr factor-2) (merge (stream-cdr factor-3) (stream-cdr factor-5)))))
;(list (stream-ref S 0) (stream-ref S 1) (stream-ref S 2) (stream-ref S 3) (stream-ref S 4))
;(list (stream-ref S 5) (stream-ref S 6) (stream-ref S 7) (stream-ref S 8) (stream-ref S 9))
;(list (stream-ref S 10) (stream-ref S 11) (stream-ref S 12) (stream-ref S 13) (stream-ref S 14))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.57
'(ex 3.57)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; is it just O(n) with memo-proc'ed Fib as we are remembering all the additions leading to the final one instead of recomputing them each time?
; what if no memo-proc? then it is 1+2+3+4+5+6 - sum of arithmetic series (1+n)*n/2 = circa O(n^2)
; but it appears there is even more slowness (exponential) it is O(fi^n)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.58
'(ex 3.58)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Give an interpretation of the stream computed by the following

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))

; (Quotient is a primitive that returns the integer quotient of two integers.) e.g. (quotient 7 2) is 3
; What are the successive elements produced by (expand 1 7 10)? What is produced by (expand 3 8 10)?



(define a2 (expand 3 8 10))
; 3 7 5 0 0 0 ...
(list (stream-ref a2 0) (stream-ref a2 1) (stream-ref a2 2) (stream-ref a2 3) (stream-ref a2 4))
(list (stream-ref a2 5) (stream-ref a2 6) (stream-ref a2 7) (stream-ref a2 8) (stream-ref a2 9))
(list (stream-ref a2 10) (stream-ref a2 11) (stream-ref a2 12) (stream-ref a2 13) (stream-ref a2 14))


; 3 7 10
; 2 7 10
; 6 7 10
; 4 7 10
; 1 4 2 8 5 ...

(define a1 (expand 1 7 10))

(list (stream-ref a1 0) (stream-ref a1 1) (stream-ref a1 2) (stream-ref a1 3) (stream-ref a1 4))
(list (stream-ref a1 5) (stream-ref a1 6) (stream-ref a1 7) (stream-ref a1 8) (stream-ref a1 9))
(list (stream-ref a1 10) (stream-ref a1 11) (stream-ref a1 12) (stream-ref a1 13) (stream-ref a1 14))

; this algo will be producing of stream of numbers which are quotients of num*den, the latter being calculated as remainder of previous num*den
; hnce for a1 it will produce numbers from 1 - 9 and then repeat itself (so kind of random generated 1-9)
; in a2 at som point the remainder becomes o, and hence all subsequent quotients will also be 0...

