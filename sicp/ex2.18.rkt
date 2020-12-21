#lang racket
;ex2.18


;recreate:
(define (reverse2 list1)
  (let ((n (- (length list1) 1))
        (temp (list)))
    (define (iter temp i)
      (define temp1 (append temp (list (list-ref list1 i))))
      (if (= i 0)
          temp1
          (iter temp1 (- i 1))))
    (iter temp n)))


;(length (list 1 2 3))
;(list-ref (list 1 2 3) 2)
;(append (list 1 2 3) (list 4 6 8))
;(list)

;
(reverse  (list 1 4 9 16 25))
(reverse2 (list 1 4 9 16 25))