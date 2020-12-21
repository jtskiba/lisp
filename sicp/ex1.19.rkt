#lang racket

; fib recur
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; fib iter
(define (fibi n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


; to calculate this you need to take the transformation Tpq and apply it twice
;Tpq:
;a<-bp+aq+ap <- a(p+q)+b(q)
;b<-bp+ap    <- a(q)  +b(p)

;T.PQ (when applied twice the above) you get:
;a<-a(2pq+p2+2q2) + b(2pq+q2)
;b<-a(2pq+q2)     + b(p2+q2)

;and by inspection you can see (mainly from b<-...) that q=2pq+q2 and p=p2+q2 (and it also works when you plug those into a<-....)


; fib iter - fast (log steps)
(define (fibf n)
(fib-iterf 1 0 0 1 n))
(define (fib-iterf a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iterf a
                    b
                    (+ (*   p p) (* q q))    ; compute p’
                    (+ (* 2 p q) (* q q))    ; compute q’
                    (/ count 2)))
        (else (fib-iterf (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))))



;(fib 100)      ; this one at 100 takes toooo long
;(fibi 1000000) ; this one at 1000000 takes about 30 sec
;(fibf 1000000) ; this one at 1000000 takes about 1 sec