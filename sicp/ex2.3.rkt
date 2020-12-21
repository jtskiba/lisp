#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment point)
  (car point))

(define (end-segment point)
  (cdr point))

(define (midpoint-segment segment)
  (cons (/ (+ (car (car segment)) (car (cdr segment))) 2)
        (/ (+ (cdr (car segment)) (cdr (cdr segment))) 2)))

;representation upper left and bottom right points
(define (rectangle1 point1 point2)
  (cons point1 point2))

;representation top left and width heigth
(define (rectangle2 point width height)
  (cons point (cons (+ (car point) width) (- (cdr point) height))))



(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define p1  (make-point 12 30))
(define p2  (make-point 18 40))
(define seg (make-segment p1 p2))
(define midseg (midpoint-segment seg))

(define (rect-area rectangle)
  (* (abs (- (x-point (cdr rectangle)) (x-point (car rectangle))))
     (abs (- (y-point (cdr rectangle)) (y-point (car rectangle))))))

(define (rect-peri rectangle)
  (* (+ (abs (- (x-point (cdr rectangle)) (x-point (car rectangle))))
        (abs (- (y-point (cdr rectangle)) (y-point (car rectangle)))))
  2))


(define rec1 (rectangle1 p1 p2))
(define rec2 (rectangle2 p1 6 10))
(rect-area rec1)
(rect-area rec2)
(rect-peri rec1)
(rect-peri rec2)





