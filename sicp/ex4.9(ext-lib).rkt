#lang racket/base
(require dyoo-while-loop)

 
;(while (not (string=? (read-line)
;                      "quit"))
;  (printf "quit?  "))


(define i 1)
(while (<= i 10) (begin (display i) (set! i (+ i 1))))



