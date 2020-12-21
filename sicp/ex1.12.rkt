#lang sicp
(define (algo_rec n)
  (cond ((< n 3) n)
        ( else (+  (     algo_rec (- n 1)  )
                  (* 2 (algo_rec (- n 2) ))
                  (* 3 (algo_rec (- n 3) ))
                  ))))

(define (algo_ite n)
  (if (< n 3)
      n
      (iteration 2 1 0 n)))


(define (iteration a b c count)
  (if (= count 2)
      a
      (iteration (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(algo_ite 10)
(algo_rec 10)
;you can see speed difference if you use say n=32
