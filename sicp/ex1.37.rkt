#lang sicp

;ex1.37
;iterative
(define (cont-frac n d k)
  (let ((i 1))
    (define (frac n d i k)
      (/ (n i) (+ (d i)
                  (if (>= i k)
                      0
                      (frac n d (+ 1 i) k)))))
    (frac n d 1 k)))

;recursive
(define (cont-frac-rec n d k)
  (if (= k 1)
      (/ (n k) (d k))
      (/ (n k)
         (+ 1 (cont-frac-rec n d (- k 1))))))


;check if rec and iter produce the same
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           7)

(cont-frac-rec (lambda (i) 1.0)
               (lambda (i) 1.0)
               7)

;continue with iterative...
(define (golden-ratio t)
  (/ 1 (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  t)))

(define (find-i tolerance)
  (let ((i 1)
        (gr 1.61803398875)) ;this is the actual golden ratio
    (define (find-iter i tolerance)
      (if (< (abs (- (golden-ratio i) gr)) tolerance)
          i
          (find-iter (+ 1 i) tolerance)))
    (find-iter i tolerance)))

(find-i 0.00001)
(golden-ratio (find-i 0.00001))
    

    




