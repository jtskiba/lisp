#lang sicp

;results of ex1.16
(define (fast-expt b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (fast-expt-iter (square b) (/ counter 2) product))
        (else (fast-expt-iter b (- counter 1) (* b product)))))

;results of ex1.17
(define (even? n)
  (= (remainder n 2) 0))

(define (double z) (+ z z))
(define (halve z) (/ z 2))
(define (square z) (fast-* z z))

;ex1.17 - my solution
(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))


;ex1.18 - Now come up with solution to come up with iterative multiplication that is using log(b) steps
(define (fast* a b)
  (fast*iter a b 0))
(define (fast*iter a counter sum)
  (cond ((= counter 0) sum)
        ((even? counter) (fast*iter (double a) (halve counter) sum))
        (else (fast*iter a (- counter 1) (+ a sum)))))

;(fast-expt 3 4)
(fast-* 31 101)
(fast* 31 101)