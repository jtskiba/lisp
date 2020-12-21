#lang racket
;(require (prefix-in rkt: racket))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.50
'(ex 3.50)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(*) https://wizardbook.wordpress.com/2010/12/20/exercise-3-50/ 

;(define (cons-stream a b) ;this will crash racket, so had to use the one below from Wizard book
;  (cons a (delay b)))

;(define (delay exp) ; this version of delay from book does not work on racket; used racket library one
;  (lambda () exp))

;(define (force delayed-object) ; defining this separately still worked, but we can remove this as
;  (delayed-object))    ;.... as delay is also defined in the racket library (base)
; however, i had to remove the above for ex3.51 as it was unable to evaluta the stream-ref

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

(stream-map square (cons-stream 2 3))
(stream-map2 + (cons-stream 2 3) (cons-stream 5 6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.51
'(ex 3.51)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(stream-enumerate-interval 0 10)

(define x 
  (stream-map 
   show 
   (stream-enumerate-interval 0 10)))


(stream-ref x 5)
;(stream-ref display 5)

(stream-ref x 7)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ex.3.52
'(ex 3.52)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))

sum ;(1 2 3 4 5 6 7 8 9 10 11...) -> (1 3 6 10 15 21 28 36 45 55 66 ...)

(define y (stream-filter even? seq))
y ;(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20...)
; -> (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210..)
; -> even (6 10 28 36 66 78 120 136...)
sum

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))

z; even (6 10 21 28 36 ...) -> (F T F F F) sum should be 10 - YES!
sum
y
(stream-ref y 7) ;(6 10 28 36 66 78 120 136...) 7 is the 8th one
sum
(display-stream z) ; (10 15 45 55 105 120 190 210) and sum should be 10? (no! it is 210!)
sum


(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

;(define (delay exp)
;  (memo-proc (lambda () exp)))

; more on the impact on memo-proc in https://wizardbook.wordpress.com/2010/12/20/exercise-3-52/





