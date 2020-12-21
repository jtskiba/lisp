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

(define (cube x) (* x x x))
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

(sum cube 1 inc 3)
(sum2 cube 1 inc 3)
(sum recip 1 inc 3)
(sum2 recip 1 inc 3)