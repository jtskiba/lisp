#lang sicp

;ex1.38
;iterative
(define (cont-frac n d k)
  (let ((i 1))
    (define (frac n d i k)
      (/ (n i) (+ (d i)
                  (if (>= i k)
                      0
                      (frac n d (+ 1 i) k)))))
    (frac n d 1 k)))


(define (fn-n i) 1)

(define (fn-d i)
  (if (= 2 (modulo i 3))
      (* 2.0
         (+ (quotient i 3)
            1))
      1))

(define (euler k)
  (+ 2 (cont-frac fn-n fn-d k)))


(euler 15)
; actual euler's number is 2.7182818284590452353602874713527..


    

    




