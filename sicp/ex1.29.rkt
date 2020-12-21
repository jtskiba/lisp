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

(integral cube 0 1 0.001)
(integral cube 0 1 0.01)

; ex 1.29 (ATTEMPT 1) same integration but using Simpson's rule instead (should be more accurate) - my version - looks like iterative though -
; so perhaps like ex1.30 but made overcomplicated
(define (sum-simp term a next b n)
  (sum-iter term a next b n 0))
(define (sum-iter term a next b n it)
  (if (> it n)
      0.0
      (+ (* (coeff it n)
            (term a))
         (sum-iter term (next a) next b n (+ 1 it)))))

(define (coeff i n)
  (if (< i n) 
   (cond ((= i 0) 1.0)
         ((even? i) 2.0)
         (else 4.0))
   1.0))
  

(define (integral-simpson f a b n)
  (define (h) (/ (- b a) n))
  (define (add-kh x) (+ x (h)))
  (* (sum-simp f a add-kh b n)
     (/ (h) 3.0)))

(integral-simpson cube 0 1 100)
(integral-simpson cube 0 1 1000)



; ex 1.29 (ATTEMPT 2) same integration but using Simpson's rule instead (should be more accurate) - my version - looks like iterative though -
; so perhaps like ex1.30 but made overcomplicated
(define (sum-simp2 term a next k b)
  (if (> a b)
      0
      (+ (term a)
         (sum-simp2 term (next a k) next k b))))

(define (integral-simpson2 f a b n)
  (define (coef i n)
    (if (< i n) 
        (cond ((= i 0) 1.0)
              ((even? i) 2.0)
              (else 4.0))
        1.0))
  (define (h) (/ (- b a) n))
  (define (add-kh x k) (+ x (* k (h))))
  (* (+ (     sum-simp2 f a            add-kh n b)
        (* 4 (sum-simp2 f (add-kh a 1) add-kh 2 b))
        (* 2 (sum-simp2 f (add-kh a 2) add-kh 2 b)))
        (/ (h) 3.0))) 

(integral-simpson2 cube 0 1 100)
(integral-simpson2 cube 0 1 1000)
