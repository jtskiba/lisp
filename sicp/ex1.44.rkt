#lang sicp

;; ex1.44
(define (inc t) (+ 1 t))
(define (square t) (* t t))

(define (compose f g)
  (lambda (x)
    (f (g x))))

;((compose square inc) 6)

(define (repeated f n)
  (lambda (x)
    (define (compose-it f1 f2 i)
      (if (= i 2)
          ((compose f1 f2) x)
          (compose-it (compose f1 f2) f2 (- i 1))))
    (compose-it f f n)))



(define (average a b c)
  (/ (+ a b c)  3.0))

(define dx 0.1)

(define (smooth f)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

((repeated square 2) 3)
((repeated square 3) 3)
((repeated square 4) 3)

((repeated (smooth square) 2) 3)
((repeated (smooth square) 3) 3)
((repeated (smooth square) 4) 3)

;((repeated square 2) 5)

;((n-smoothed square 2) 5)    

;((repeated (smooth square) 5) 2)



