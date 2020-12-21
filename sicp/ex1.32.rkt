#lang sicp
; per book - using numerical approximation from the main text
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (identity x) x)
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (recip x) (/ 1.0 x))

;(integral cube 0 1 0.001)
;(integral cube 0 1 0.01)

;ex1.30 - making iterative version of sum (i did one already in ex1.29 but it was perhaps not too streamlined)

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc x) (+ 1 x))

;(sum cube 1 inc 3)
;(sum2 cube 1 inc 3)
;(sum recip 1 inc 3)
;(sum2 recip 1 inc 3)

;ex1.31 - recursive (a)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; and iterative - (b)
(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial x)
  (define (identity x) x)
  (define (inc x) (+ 1 x))
  (define (product2 term a next b)
    (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (* (term a) result))))
    (iter a 1))
  (product2 identity 1 inc x))


  
;(product identity 1 inc 3)
;(product2 identity 1 inc 3)
;(product square 2 inc 5)
;(product2 square 2 inc 5)
;(factorial 4)

;calc Pi using formula given by Wallis
(define (Pi) ;may be wrong
  (define (precision) 170)
  (define (inc2 x) (+ 2 x))
  (/ (* (/ 2.0 (+ 1 (precision)))
        (product square 2.0 inc2 (precision)))
     (product square 3.0 inc2 (+ (precision) 0))))

(define (Pi2)
  (define (precision) 170)
  (define (inc2 x) (+ 2 x))
  (/ (* (/ 2.0 (precision))
        (product square 2.0 inc2 (precision)))
     (product square 3.0 inc2 (precision))))


(define (Pi3)
  (define (precision) 170)
  (define (inc2 x) (+ 2 x))
  (/ (product square 2.0 inc2 (precision)) 
     (* 0.5 (precision) (product square 3.0 inc2 (precision)))))

;(Pi)
;(Pi2)
;(Pi3)

;ex1.32

(define (accumulate combiner base term a next b)
  (if (> a b)
      base
      (combiner (term a)
         (accumulate combiner base term (next a) next b))))

(define (accumulate2 combiner base term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a base))


(product identity 1 inc 3)
(accumulate * 1 identity 1 inc 3)
(accumulate2 * 1 identity 1 inc 3)

(sum identity 1 inc 3)
(accumulate + 0 identity 1 inc 3)
(accumulate2 + 0 identity 1 inc 3)