#lang sicp
;ex1.27


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
(define (expmod base exp m) ; previous version
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

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


;(fermat-test 11) ;yields #t so true, it is a prime based on fermat test

;lets try Carmichael numbers which fool fermat-test (so it says it is prime, but is not in reality)
; 561, 1105, 1729, 2465, 2821, and 6601.
(define (fermat-test2 n)
  (fermat-iter n 1))
(define (fermat-iter n a)
  (if (< a n)
      (cond ((= (expmod a n n) a) (fermat-iter n (+ 1 a)))
            (else #f))
      #t))
      
 
(fermat-test2 11)
(fermat-test2 561)
(fermat-test2 1105)
(fermat-test2 1729)
(fermat-test2 2465)
(fermat-test2 2821)
(fermat-test2 6601)
