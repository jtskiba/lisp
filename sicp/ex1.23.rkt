#lang sicp
;ex1.22
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (next z)
  (if (= z 2)
      (+ 1 z)
      (+ 2 z)))
      

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x) (* x x))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes from to)
  (if (> to from)
      (search-iter from to)))

(define (search-iter from to)
  (timed-prime-test from)
  (search-for-primes (+ 1 from) to))

;(timed-prime-test 1203571)
;(timed-prime-test 120)
(search-for-primes 1000 1020)
(newline)
(search-for-primes 10000 10038)
(newline)
(search-for-primes 100000 100044)
(newline)
(search-for-primes 1000000 1000038)
(newline)
(search-for-primes 100000000 100000040)



;previous results - ex1.22
;1009 *** 2
;1013 *** 3
;1019 *** 3
;
;10007 *** 9
;10009 *** 9
;10037 *** 9
;
;100003 *** 29
;100019 *** 29
;100043 *** 29
;
;1000003 *** 86
;1000033 *** 85
;1000037 *** 85
;
;100000007 *** 858
;100000037 *** 848
;100000039 *** 848
