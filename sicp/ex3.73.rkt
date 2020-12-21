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
  (stream-map2 + s1 s2))

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
;(list (stream-ref a2 0) (stream-ref a2 1) (stream-ref a2 2) (stream-ref a2 3) (stream-ref a2 4))
;(list (stream-ref a2 5) (stream-ref a2 6) (stream-ref a2 7) (stream-ref a2 8) (stream-ref a2 9))
;(list (stream-ref a2 10) (stream-ref a2 11) (stream-ref a2 12) (stream-ref a2 13) (stream-ref a2 14))


; 3 7 10
; 2 7 10
; 6 7 10
; 4 7 10
; 1 4 2 8 5 ...

(define a1 (expand 1 7 10))

;(list (stream-ref a1 0) (stream-ref a1 1) (stream-ref a1 2) (stream-ref a1 3) (stream-ref a1 4))
;(list (stream-ref a1 5) (stream-ref a1 6) (stream-ref a1 7) (stream-ref a1 8) (stream-ref a1 9))
;(list (stream-ref a1 10) (stream-ref a1 11) (stream-ref a1 12) (stream-ref a1 13) (stream-ref a1 14))

; this algo will be producing of stream of numbers which are quotients of num*den, the latter being calculated as remainder of previous num*den
; hnce for a1 it will produce numbers from 1 - 9 and then repeat itself (so kind of random generated 1-9)
; in a2 at som point the remainder becomes o, and hence all subsequent quotients will also be 0...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.59
'(ex 3.59)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART 1 ;;;;;;;;;;


;ones
;integers

(define (f-half stream)
  (cons-stream (/ 1 (stream-car stream))
               (f-half (stream-cdr stream))))

(define (multiply-streams s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (multiply-streams (stream-cdr s1)
                                 (stream-cdr s2))))

(define (integrate-series0 stream c)
  (cons-stream c (multiply-streams stream (f-half integers))))

;(define a3 (integrate-series0 ones 3))
;(define a4 (integrate-series0 integers 3))
;(define a4 (multiply-streams a3 ones))

;(list (stream-ref a3 0) (stream-ref a3 1) (stream-ref a3 2) (stream-ref a3 3) (stream-ref a3 4))
;(list (stream-ref a4 0) (stream-ref a4 1) (stream-ref a4 2) (stream-ref a4 3) (stream-ref a4 4))

;;; PART 2 ;;;;;;;;;;
; from part 1:
(define (integrate-series stream)
  (multiply-streams stream (f-half integers)))

(define exp-series
  (cons-stream 
   1 (integrate-series exp-series)))

;(list (stream-ref exp-series 0) (stream-ref exp-series 1) (stream-ref exp-series 2) (stream-ref exp-series 3) (stream-ref exp-series 4))

;Show how to generate the series for sine and cosine, starting from the facts that the derivative of sine is cosine and the derivative of cosine is the negative of sine:

(define cosine-series 
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;cosine-series
;(list (stream-ref cosine-series 0) (stream-ref cosine-series 1) (stream-ref cosine-series 2) (stream-ref cosine-series 3) (stream-ref cosine-series 4) (stream-ref cosine-series 5))

;sine-series
;(list (stream-ref sine-series 0) (stream-ref sine-series 1) (stream-ref sine-series 2) (stream-ref sine-series 3) (stream-ref sine-series 4) (stream-ref sine-series 5))
;WOW!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.60
'(ex 3.60)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s2)
                  (stream-car s1))
               (add-streams (mul-series s1 (stream-cdr s2))
                            (scale-stream (mul-series s2 (stream-cdr s1)) -1))))

(define summed (add-streams (mul-series cosine-series cosine-series) (mul-series sine-series sine-series)))
;(define summed (add-streams ones integers))
;(list (stream-ref summed 0) (stream-ref summed 1) (stream-ref summed 2) (stream-ref summed 3) (stream-ref summed 4) (stream-ref summed 5))

;(define m1 (mul-series cosine-series cosine-series))
;(list (stream-ref m1 0) (stream-ref m1 1) (stream-ref m1 2) (stream-ref m1 3) (stream-ref m1 4) (stream-ref m1 5))

;(define m2 (scale-stream ones -1))
;(list (stream-ref m2 0) (stream-ref m2 1) (stream-ref m2 2) (stream-ref m2 3) (stream-ref m2 4) (stream-ref m2 5))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.61
'(ex 3.61)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mult from https://wizardbook.wordpress.com/2010/12/20/exercise-3-60/
(define (mul-series2 s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) 
                                          (stream-car s2))
                            (mul-series2 s1 
                                        (stream-cdr s2)))))

;(define summed2 (add-streams (mul-series2 cosine-series cosine-series) (mul-series sine-series sine-series)))
;(list (stream-ref summed2 0) (stream-ref summed2 1) (stream-ref summed2 2) (stream-ref summed2 3) (stream-ref summed2 4) (stream-ref summed2 5))



(define (invert-series stream)
  (cons-stream 1
              (scale-stream (mul-series2 (stream-cdr stream) (invert-series stream)) -1)))

;(define inv (invert-series exp-series))
;(list (stream-ref inv 0) (stream-ref inv 1) (stream-ref inv 2) (stream-ref inv 3) (stream-ref inv 4) (stream-ref inv 5))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.62-skipped
'(ex 3.62-skipped)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3.5.3 Exploiting the Stream Paradigm
'(Exploiting the Stream Paradigm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

;(display-stream (sqrt-stream 2))
;(stream-ref (sqrt-stream 2) 30)

(define (pi-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

(define pi-stream
  (scale-stream 
   (partial-sums (pi-summands 1)) 4))

;(display-stream pi-stream)
;(stream-ref pi-stream 150)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sₙ₋₁
        (s1 (stream-ref s 1))     ; Sₙ
        (s2 (stream-ref s 2)))    ; Sₙ₊₁
    (cons-stream 
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

;(display-stream 
; (euler-transform pi-stream))

;;; we create a stream of streams (a structure we’ll call a tableau) ;;;;

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;(display-stream 
; (accelerated-sequence euler-transform
;                       pi-stream))

;test of speed:
;(stream-ref pi-stream 6)
;(stream-ref (euler-transform pi-stream) 6)
;(stream-ref (accelerated-sequence euler-transform
;                       pi-stream) 6) ; this is so good that after 10th is returns +nan.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.63
'(ex 3.63)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (sqrt-stream-PREV x)
  (define guesses
    (cons-stream 
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(define (sqrt-stream-SUGG x) ; this version stips out the overarching "define guesses"
  (cons-stream 
   1.0 (stream-map
        (lambda (guess)
          (sqrt-improve guess x))
        (sqrt-stream-SUGG x))))


;(stream-ref (sqrt-stream-PREV 2) 4)
;(stream-ref (sqrt-stream-SUGG 2) 4)

; is this about memory and how by calling the headline definition sqrt-stream all the time
; we are keep adding new enironments instead, wasting memory?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.64
'(ex 3.64)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Write a procedure stream-limit that takes as arguments a stream and a number (the tolerance)

(define (stream-limit s t)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) t)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) t)))


(define (sqrt-sicp x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;(sqrt-sicp 2 0.1)
;(sqrt-sicp 2 0.01)
;(sqrt-sicp 2 0.001)
;(sqrt-sicp 2 0.000001)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.65
'(ex 3.65)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ln2-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (scale-stream 
   (partial-sums (ln2-summands 1)) 1)) ; no need for scaling

;(display-stream pi-stream)
;(stream-ref ln2-stream 150)
;(list (stream-ref ln2-stream 0) (stream-ref ln2-stream 1) (stream-ref ln2-stream 2)
;      (stream-ref ln2-stream 3) (stream-ref ln2-stream 4) (stream-ref ln2-stream 5)
;      (stream-ref ln2-stream 6) (stream-ref ln2-stream 7) (stream-ref ln2-stream 8))

;test of speed:
;(stream-ref ln2-stream 6)
;(stream-ref (euler-transform ln2-stream) 6)
;(stream-ref (accelerated-sequence euler-transform ln2-stream) 6) ; accurate already!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Infinite streams of pairs
'(Infinite streams of pairs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


(define int-pairs (pairs integers integers))

(define pair-primes
  (stream-filter 
   (lambda (pair)
     (prime? (+ (car pair) (cadr pair))))
   int-pairs))

; pairs such that sum of the two ints are prime:
;(list (stream-ref pair-primes 0) (stream-ref pair-primes 1) (stream-ref pair-primes 2)
;      (stream-ref pair-primes 3) (stream-ref pair-primes 4) (stream-ref pair-primes 5)
;      (stream-ref pair-primes 6) (stream-ref pair-primes 7) (stream-ref pair-primes 8))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.66
'(ex 3.66)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Examine the stream (pairs integers integers). Can you make any general comments about
; the order in which the pairs are placed into the stream? For example,
; approximately how many pairs precede the pair (1, 100)? the pair (99, 100)? the pair (100, 100)?
;(If you can make precise mathematical statements here, all the better.
; But feel free to give more qualitative answers if you find yourself getting bogged down.)


(define lista (list (stream-ref int-pairs 0) (stream-ref int-pairs 1 ) (stream-ref int-pairs 2 )
      (stream-ref int-pairs 3) (stream-ref int-pairs 4 ) (stream-ref int-pairs 5 )
      (stream-ref int-pairs 6) (stream-ref int-pairs 7 ) (stream-ref int-pairs 8 )
      (stream-ref int-pairs 9) (stream-ref int-pairs 10) (stream-ref int-pairs 11)
      (stream-ref int-pairs 12) (stream-ref int-pairs 13) (stream-ref int-pairs 14)
      (stream-ref int-pairs 15) (stream-ref int-pairs 16) (stream-ref int-pairs 17)
      (stream-ref int-pairs 18) (stream-ref int-pairs 19) (stream-ref int-pairs 20)))
; '(1:(1 1) 2:(1 2) 3:(2 2) 4:(1 3) 5:(2 3) 6:(1 4) 7:(3 3) 8:(1 5) 9:(2 4) 10:(1 6) 11:(3 4) 12:(1 7)
;   13:(2 5) 14:(1 8) 15:(4 4) 16:(1 9) 17:(2 6) 18:(1 10) 19: (3 5) 20: (1 11) 21: (2 7))

; Qualititative is that the top row will be done most frequently, as we can see from elts 2, 4, 6, ... i.e. (12) (13) (14) (15) ....
; so they will show at (x y) -> then  (1 1)(1 2)(1 3)(1 4)(1 5)(1 6)(1 7) is 1 2 4 6 8 10
; FOR x=1 & y=1: 1
; FOR x=1 & y>1: 2*x*(y-1)
; the iunterleaved one is another triangle, so then top row of that will be prioritise in the alternating ones,
; i.e. ended up being shown every 4th time
; x=2 y=2: 11 22 33 44 55 => 1,3,7,15
;                                                                                         y=x  y=x+1  y=x+2  y=x+3
;                                                                                  
; and the first row  (1 1) (1 2) (1 3) (1 4) (1 5) (1 6) is 1 , 2 , 4 , 6 , 8 , 10 so x=1:  1    +1     +2     +2
; and the second row (2 2) (2 3) (2 4) (2 5) (2 6) (2 7) is 3 , 5 , 9 , 13, 17, 21 so x=2: +2    +2     +4     +4
; and the third row  (3 3) (3 4) (3 5) (3 6) (3 7) (3 8) is 7 , 11, 19, 27, 35, 43 so x=3: +4    +4     +8     +8
; and the fourth row (4 4) (4 5) (4 6) (4 7) (4 8) (4 9) is 15, 23, 39, 55, 71, 87 so x=4: +8    +8    +16    +16

;      y=x  y=x+1  y=x+2  y=x+3
; x=1:  1    +1     +2     +2
; x=2: +2    +2     +4     +4
; x=3: +4    +4     +8     +8
; x=4: +8    +8    +16    +16  

;      y=x  y=x+1  y=x+2  y=x+3
; x=1:  1     2      4      6
; x=2:  3     5      9     13
; x=3:  7    11     19     27
; x=4: 15    23     39     55  

;for x=y  : [(2^x)-1]
;for x=y-1: [(2*x)+1]
;for x<y-1: (y-x+1)*[(2^x)-1]
;for all:   [(2^x)-1] + {(y-x)*(2^x)}-{2^(x-1)}
; e.g. (x = 4, y = 6), should be 39, is it? [(2^4)-1]+{(6-4)*(2^4)}-{2^(4-1)} = 15 + 32 - 8 = 39 ok!
; can we simplify this formula?

(define (calc x y)
  (+ (- (expt 2 x) 1)
     (* (- y x)
        (expt 2 x))
     (* -1
        (expt 2 (- x 1)))))

;(calc 2 7)

;(map
; (lambda(pair)
;   (calc (car pair) (cadr pair)))
; lista)
; ok - not working for some, but good enough for this purpose.

; 2^(x-1)   or    2^(x)
; for (y-x) if:  0 -> 0; 1 -> 1 and 2,3,4 -> 2 

; e.g. (x=3 y=3) -> 7  (agreed to formula)
; e.g. (x=4 y=5) -> 23 (agreed to formula)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.67 <- 01 AUg 2019
'(ex 3.67)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pairs2 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
     (pairs (stream-cdr s) (stream-cdr t)))
    (stream-map (lambda (x) 
                  (list x (stream-car t)))
                (stream-cdr s)))))

(define int-pairs2 (pairs2 integers integers))

(define lista2 (list (stream-ref int-pairs2 0) (stream-ref int-pairs2 1 ) (stream-ref int-pairs2 2 )
      (stream-ref int-pairs2 3) (stream-ref int-pairs2 4 ) (stream-ref int-pairs2 5 )
      (stream-ref int-pairs2 6) (stream-ref int-pairs2 7 ) (stream-ref int-pairs2 8 )
      (stream-ref int-pairs2 9) (stream-ref int-pairs2 10) (stream-ref int-pairs2 11)
      (stream-ref int-pairs2 12) (stream-ref int-pairs2 13) (stream-ref int-pairs2 14)
      (stream-ref int-pairs2 15) (stream-ref int-pairs2 16) (stream-ref int-pairs2 17)
      (stream-ref int-pairs2 18) (stream-ref int-pairs2 19) (stream-ref int-pairs2 20)))

;lista2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.68
'(ex 3.68)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pairs3 s t) ; this will not print anything - just loop without stopping until out of mem
  (interleave
   (stream-map
    (lambda (x) 
      (list (stream-car s) x))
    t)
   (pairs3 (stream-cdr s)
           (stream-cdr t))))

(define (pairs-original s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map
     (lambda (x) 
       (list (stream-car s) x))
     (stream-cdr t))
    (pairs-original (stream-cdr s)
                    (stream-cdr t)))))

;(define int-pairs3 (pairs3 integers integers))

;(define lista3 (list (stream-ref int-pairs3 0) (stream-ref int-pairs3 1 ) (stream-ref int-pairs3 2 )
;      (stream-ref int-pairs3 3) (stream-ref int-pairs3 4 ) (stream-ref int-pairs3 5 )
;      (stream-ref int-pairs3 6) (stream-ref int-pairs3 7 ) (stream-ref int-pairs3 8 )
;      (stream-ref int-pairs3 9) (stream-ref int-pairs3 10) (stream-ref int-pairs3 11)
;      (stream-ref int-pairs3 12) (stream-ref int-pairs3 13) (stream-ref int-pairs3 14)
;      (stream-ref int-pairs3 15) (stream-ref int-pairs3 16) (stream-ref int-pairs3 17)
;      (stream-ref int-pairs3 18) (stream-ref int-pairs3 19) (stream-ref int-pairs3 20)))

; this is because the interleave has got recursive call, and so will need that at least
; first element. but there is no first element in the revised version so the calls will
; go in circles without second part of interleave being executed.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.69
'(ex 3.69)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Write a procedure triples that takes three infinite streams, S, T, and U, and produces
; the stream of triples (Si,Tj,Uk) such that i≤j≤k. Use triples to generate the stream
; of all Pythagorean triples of positive integers, i.e., the triples (i,j,k) such that i≤j
; and i2+j2=k2.

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map
     (lambda (x) 
       (cons (stream-car s) x))
     (pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))

(define int-triples (triples integers integers integers))

;(define int-triples (cons-stream (list 1 2 3) (cons-stream (list 3 4 5) '())))

(define pyth-triple
  (stream-filter 
   (lambda (triple)
     (= (+ (expt (car triple) 2) (expt (cadr triple) 2)) (expt (caddr triple) 2)))
   int-triples))

(define p-t-list
  (list
   (stream-ref pyth-triple 0) (stream-ref pyth-triple 1 ) (stream-ref pyth-triple 2 )))

;p-t-list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.70
'(ex 3.70)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define factor-2 
;  (cons-stream 1 (scale-stream factor-2 2)))
;
;(define factor-3 
;  (cons-stream 1 (scale-stream factor-3 3)))
;
;(define factor-5 
;  (cons-stream 1 (scale-stream factor-5 5)))

;(list (stream-ref factor-3 0) (stream-ref factor-3 1) (stream-ref factor-3 2) (stream-ref factor-3 3) (stream-ref factor-3 4))

;merge two streams and remove repetitions
(define (merge-weighted s1 s2 weight-fn)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
             (cond ((<= (weight-fn s1car) (weight-fn s2car))
                    (cons-stream 
                     s1car 
                     (merge-weighted (stream-cdr s1) s2 weight-fn)))
                   ((> (weight-fn s1car) (weight-fn s2car))
                    (cons-stream 
                     s2car 
                     (merge-weighted s1 (stream-cdr s2) weight-fn)))
                   ;(else
                   ; (cons-stream 
                   ;  s1car
                   ;  (merge-weighted
                   ;   (stream-cdr s1)
                   ;   (stream-cdr s2)
                   ;  weight-fn)))
                   )))))

(define (weight-1 pair)
  (+ (car pair) (cadr pair)))

(define (weight-2 pair)
  (+ (* 2 (car pair))
     (* 3 (cadr pair))
     (* 5 (car pair) (cadr pair))))

;(define ordered-1 ;that's my weighted pairs
;  (merge-weighted int-pairs weight-1))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map
     (lambda (x) 
       (list (stream-car s) x))
     (stream-cdr t))
    (weighted-pairs (stream-cdr s)
                    (stream-cdr t)
                    weight)
    weight)))

;(define int-cond1 (weighted-pairs integers integers weight-1))
;(define int-cond2 (weighted-pairs integers integers weight-2))


;(list (stream-ref int-pairs 0) (stream-ref int-pairs 1 ) (stream-ref int-pairs 2 )
;      (stream-ref int-pairs 3) (stream-ref int-pairs 4 ) (stream-ref int-pairs 5 )
;      (stream-ref int-pairs 6) (stream-ref int-pairs 7 ) (stream-ref int-pairs 8 )
;      (stream-ref int-pairs 9) (stream-ref int-pairs 10) (stream-ref int-pairs 11)
;      (stream-ref int-pairs 12) (stream-ref int-pairs 13) (stream-ref int-pairs 14)
;      (stream-ref int-pairs 15) (stream-ref int-pairs 16) (stream-ref int-pairs 17)
;      (stream-ref int-pairs 18) (stream-ref int-pairs 19) (stream-ref int-pairs 20))
;
;(list (stream-ref int-cond1 0) (stream-ref int-cond1 1 ) (stream-ref int-cond1 2 )
;      (stream-ref int-cond1 3) (stream-ref int-cond1 4 ) (stream-ref int-cond1 5 )
;      (stream-ref int-cond1 6) (stream-ref int-cond1 7 ) (stream-ref int-cond1 8 )
;      (stream-ref int-cond1 9) (stream-ref int-cond1 10) (stream-ref int-cond1 11)
;      (stream-ref int-cond1 12) (stream-ref int-cond1 13) (stream-ref int-cond1 14)
;      (stream-ref int-cond1 15) (stream-ref int-cond1 16) (stream-ref int-cond1 17)
;      (stream-ref int-cond1 18) (stream-ref int-cond1 19) (stream-ref int-cond1 20))
;
;(define check
;  (list (stream-ref int-cond2 0) (stream-ref int-cond2 1 ) (stream-ref int-cond2 2 )
;        (stream-ref int-cond2 3) (stream-ref int-cond2 4 ) (stream-ref int-cond2 5 )
;        (stream-ref int-cond2 6) (stream-ref int-cond2 7 ) (stream-ref int-cond2 8 )
;        (stream-ref int-cond2 9) (stream-ref int-cond2 10) (stream-ref int-cond2 11)
;        (stream-ref int-cond2 12) (stream-ref int-cond2 13) (stream-ref int-cond2 14)
;        (stream-ref int-cond2 15) (stream-ref int-cond2 16) (stream-ref int-cond2 17)
;        (stream-ref int-cond2 18) (stream-ref int-cond2 19) (stream-ref int-cond2 20)))
;
;(map
; (lambda (pair)
;   (weight-2 pair))
; check)
; ok - works!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.71
'(ex 3.71)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ramanujan-weight pair)
  (+ (expt (car pair) 3) (expt (cadr pair) 3)))

;(define r-numbers (weighted-pairs integers integers ramanujan-weight))

(define (stream-until stream to)
  (define (iter stream now found mem-r mem-p)
    (let ((old-r mem-r)
          (old-p mem-p)
          (mem-r (ramanujan-weight (stream-ref stream now)))
          (mem-p (stream-ref stream now)))
      (if (= found to)
          '()
          (if (= old-r mem-r)
               (cons (list now old-p mem-p mem-r) 
                     (iter stream (+ now 1) (+ found 1) mem-r mem-p))
               (iter stream (+ now 1) found mem-r mem-p)))))
  (iter stream 0 0 0 '()))

;(stream-until r-numbers 5)

; ((61 (1 12) (9 10) 1729)
;  (111 (2 16) (9 15) 4104)
;  (250 (2 24) (18 20) 13832)
;  (331 (10 27) (19 24) 20683)
;  (449 (4 32) (18 30) 32832))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.72
'(ex 3.72)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (squares-weight pair)
  (+ (expt (car pair) 2) (expt (cadr pair) 2)))

;(define s-numbers (weighted-pairs integers integers squares-weight))

;(define (stream-until2 stream to)
;  (define (iter stream now found mem-s mem-p mem-ss mem-pp)
;    (let ((old-s mem-s)
;          (old-p mem-p)
;          (old-ss mem-ss)
;          (old-pp mem-pp)
;          (mem-s (squares-weight (stream-ref stream now)))
;          (mem-p (stream-ref stream now)))
;      (if (= found to)
;          '()
;          (if (= old-s old-ss mem-s)
;               (cons (list now old-pp old-p mem-p mem-s) 
;                     (iter stream (+ now 1) (+ found 1) mem-s mem-p old-s old-p))
;               (iter stream (+ now 1) found mem-s mem-p old-s old-p)))))
;  (iter stream 0 0
;        0 '()
;        0 '()))

;(stream-until2 s-numbers 5)

;'((125 (1 18) (6 17) (10 15) 325)
;  (165 (5 20) (8 19) (13 16) 425)
;  (252 (5 25) (11 23) (17 19) 650)
;  (281 (7 26) (10 25) (14 23) 725)
;  (327 (2 29) (13 26) (19 22) 845))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Streams as signals
'(Streams as signals)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.73
'(ex 3.73)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (RC R C dt)
  (lambda (initial integrand)
    (add-streams
     (scale-stream integrand R)      
     (integral (scale-stream integrand (/ 1 C))
               initial
               dt))))

(define RC1 (RC 5 1 0.5))

RC1

(stream-ref (RC1 0.2 integers) 0)
(stream-ref (RC1 0.2 integers) 1)
(stream-ref (RC1 0.2 integers) 2)
(stream-ref (RC1 0.2 integers) 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.74
'(ex 3.74)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sign-change-detector curr prev)
  (cond ((or
          (and (>= curr 0) (>= prev 0))
          (and (<  curr 0) (<  prev 0))) 0)
        ((and  (>= curr 0) (<  prev 0))  1)
        ((and  (<  curr 0) (>= prev 0)) -1)
        (else 2)))

(define (make-zero-crossings
         input-stream last-value)
  (cons-stream
   (sign-change-detector 
    (stream-car input-stream) 
    last-value)
   (make-zero-crossings 
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define (make-my-stream list)
  (if (null? list)
      '()
      (cons-stream (car list)
                   (make-my-stream (cdr list)))))

(define (show-stream stream to)
  (define (iter stream now)
    (if (stream-null? stream)
        '()
        (if (> now to)
            '()
            (cons (stream-car stream)
                  (iter (stream-cdr stream) (+ now 1))))))
  (iter stream 0))

(define sense-data (make-my-stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define zero-crossings 
  (make-zero-crossings sense-data 0))

(show-stream zero-crossings 11)

(define zero-crossings2
  (stream-map2 sign-change-detector 
              (stream-cdr sense-data) 
              sense-data))

(show-stream zero-crossings2 10)

