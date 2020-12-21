#lang racket
;ex2.19



;(length (list 1 2 3))
;(list-ref (list 1 2 3) 2)
;(append (list 1 2 3) (list 4 6 8))
;(list)


(define (count-change amount list-of-coins)
  (cc amount (length list-of-coins) list-of-coins))

(define (cc amount kinds-of-coins list-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1) list-of-coins)
            (cc (- amount (first-denomination 
                           kinds-of-coins
                           list-of-coins))
                kinds-of-coins
                list-of-coins)))))

(define (first-denomination kinds-of-coins list-of-coins)
  (list-ref list-of-coins (- kinds-of-coins 1))
  ;(cond ((= kinds-of-coins 1) 1)
  ;      ((= kinds-of-coins 2) 5)
  ;      ((= kinds-of-coins 3) 10)
  ;      ((= kinds-of-coins 4) 25)
  ;      ((= kinds-of-coins 5) 50))
  )

(define usd-coins (list 1 5 10 25 50))
(define gbp-coins (list 1 2 5 10 25 50 100))
(define pln-coins (list 1 2 5 10 50 100 200 500))
(define uk-coins  (list 100 50 20 10 5 2 1 0.5))
(define uk-coins2  (list 0.5 1 2 5 10 20 50 100))


(count-change 100 usd-coins)
(count-change 100 gbp-coins)
(count-change 100 pln-coins)
(count-change 100 uk-coins)
(count-change 100 uk-coins2)