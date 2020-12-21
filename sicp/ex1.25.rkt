#lang sicp
;ex1.25
; works okish on small numbers but slow (overflowing) on larger
; apparently due to this being define on large base in logarithm (not 2) which makes things harder

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
(define (expmod0 base exp m) ; previous version
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (expmod base exp m) ; ALisson Hacker's one - better on surface, but slow in practive for large numbers n
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

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

; run this, slow, so results pasted below
;(search-for-primes 1000 1020)
;(newline)
;(search-for-primes 10000 10038)
;(newline)
;(search-for-primes 100000 100044)
;(newline)
;(search-for-primes 1000000 1000038)


;results of ex1.24 (test speed roughly doubled from 1,000 to 1,000,000 using fermat test which is logn speed)
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

;results from ex1.25 - slow overflowing
;1009 *** 32
;1013 *** 19
;1017 *** 8
;1019 *** 16
;
;10007 *** 838
;10009 *** 780
;10037 *** 337
;
;100003 *** 34057
;100019 *** 35855
;100043 *** 29567
;
;1000003 *** 1327612
;1000033 *** 1285540
;1000037 *** 1004189


;this is the test of old expmod and new exp mod
;(expmod 931831 1000025 1000025) ;takes over a minute - not finished
;(expmod0 931831 1000025 1000025) ; takes 6 seconds
;hence the earlier version is more efficient, does not deal with large base ^ large n
; read wiki
