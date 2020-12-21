#lang sicp

;; below is taken from book - exercise further down

(define (average a b)
  (/ (+ a b) 2.0))

(define (square x) (* x x))

;(average 2 3)

;average damp
(define (average-damp f)
  (lambda (x) (average x (f x))))

;((average-damp square) 10)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(sqrt 10)
(sqrt2 10)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;(cube-root 3)

;Newton's method

;define derivative and small incremement
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

;((deriv cube) 5) ;so (derive cube) returns a function in the form of lambda
    
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt3 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(sqrt3 10)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt4 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(sqrt4 10)

(define (sqrt5 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(sqrt5 10)

;ex 1.40
;(define (cubic a b c)
;  (newtons-method (lambda (y) (+ (cube y) (* a (square y)) (* b y) c))
;                  1.0))
;(cubic 1 0 1)

(define (cubic a b c)
  (lambda (y)
    (+ (cube y) (* a (square y)) (* b y) c)))


(newtons-method (cubic 1 0 1) 0.5)





