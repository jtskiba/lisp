#lang sicp
; multiplication using only addition

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))


(define (even? n)
  (= (remainder n 2) 0))

(define (double z) (+ z z))
(define (halve z) (/ z 2))

;ex1.17 - my solution
(define (fast* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast* a (halve b))))
        (else (+ a (fast* a (- b 1))))))

(* 3 11)
(fast* 3 11)