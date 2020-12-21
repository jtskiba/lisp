#lang sicp
;recursive process:
(define (plus a b)
  (if (= a 0)
      b
      (inc (plus (dec a) b))))

;iterative process:
(define (plus2 a b)
  (if (= a 0)
      b
      (plus2 (dec a) (inc b))))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))
