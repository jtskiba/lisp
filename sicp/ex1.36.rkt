#lang sicp

;start paying attention

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (display count)
    (display "***")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ 1 count)))))
  (try first-guess 1))

(define (fixed-point-d f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (display count)
    (display "***")
    (display guess)
    (newline)
    (let ((next (average guess (f guess))))
      (if (close-enough? guess next)
          next
          (try next (+ 1 count)))))
  (try first-guess 1))

(define (average x y)
  (/ (+ x y) 2.0))

; ex 1.36
;no damping
(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.5)
;damping
(fixed-point-d (lambda (x) (/ (log 1000) (log x))) 1.5)




