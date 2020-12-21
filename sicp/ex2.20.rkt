#lang racket
;ex2.20

(define (same-parity . w)
  (define (give type lst)
    (define len (length lst))
    (define temp (list))
    (define (iter temp j len)
      (if (= j len)
          temp
          (if (= (remainder (list-ref lst j) 2) type)
              (iter temp
                    (+ j 1)
                    len)
              (iter (append temp
                            (list (list-ref lst j)))
                    (+ j 1)
                    len))))
    (iter temp 0 len))
  (define (give-odd  w) 1)
      
  (if (= (remainder (car w) 2) 0)
      (give 1 (cdr w))  ;even
      (give 0 (cdr w))));odd

(same-parity 1 2 3 4 5 14 15 18 19 9 7 4)

(same-parity 2 4 5 1 2 3 2 5 6 66 3 4)