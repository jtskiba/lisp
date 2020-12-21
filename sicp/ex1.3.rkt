#lang sicp
(define (sum_sq a b) (+ (* a a) (* b b) ))
(define (sq_two_biggest x y z)
        (cond ((and (> x y) (> y z)) (sum_sq x y))
              ((and (> y x) (> x z)) (sum_sq x y))
              ((and (> x z) (> z y)) (sum_sq x z))
              ((and (> z x) (> x y)) (sum_sq x z))
              (else (+ y z))))