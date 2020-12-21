#lang sicp

;ex1.39
;iterative
(define (cont-frac n d k x)
  (let ((i 1))
    (define (frac n d i k)
      (/ (n i x) (- (d i)
                  (if (>= i k)
                      0
                      (frac n d (+ 1 i) k)))))
    (frac n d 1 k)))


(define (fn-n i x)
  (if (= i 1)
      x
      (expt x 2)))

(define (fn-d i)
  (- (* 2 i) 1))


(define (tan-cf x k)
  (cont-frac fn-n fn-d k x))

(define (degree-to-rad x)
  (let ((Pi 3.14159265359))
    (* x (/ Pi 180.0))))

;(tan 3.14159265359)
;(tan (degree-to-rad 180))

(tan (degree-to-rad 50))
(tan-cf (degree-to-rad 50) 8)





    

    




