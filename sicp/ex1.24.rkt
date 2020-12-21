#lang sicp
;ex1.24
; this will use fast-prime instead so should only see doubling of time taken to calc primes between 1,000 and 1,000,000
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

;FAST-PRIME FNS
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;;;;;;;;;;;;;;;;;;; END OF FAST-PRIME FNS

; below is modified to run on fast-prime instead
(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x) (* x x))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
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

;previous results - ex1.23 (adding divisions by half of numbers only) ca.58% of time
;1009 *** 2
;1013 *** 2
;1019 *** 2
;
;10007 *** 5
;10009 *** 5
;10037 *** 5
;
;100003 *** 16
;100019 *** 16
;100043 *** 16
;
;1000003 *** 51
;1000033 *** 51
;1000037 *** 51

;results of this run - expectation confirmed (test speed roughly doubled from 1,000 to 1,000,000)
;1009 *** 2
;1013 *** 2
;1019 *** 2
;
;10007 *** 5
;10009 *** 3
;10037 *** 2
;
;100003 *** 3
;100019 *** 3
;100043 *** 3
;
;1000003 *** 3
;1000033 *** 4
;1000037 *** 4

