#lang racket
'((Norah 12) 
 (Molly 9) 
 (Anna 7) 
 (Lauren 6) 
 (Charlotte 4))

;(* (+ 23 45) (+ x 9))
(define (fact n) 
  (if (= n 1) 
      1 
      (* n (fact (- n 1)))))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;(memq 'apple '(pear banana prune))
;(memq 'apple '(x (apple sauce) y apple pear))

;ex2.53
(list 'a 'b 'c)
;'(a b c)
(list (list 'george))
;'((george))
(cdr '((x1 x2) (y1 y2)))
;'((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;'(y1 y2)
(pair? (car '(a short list)))
;#f
(memq 'red '((red shoes) (blue socks)))
;#f
(memq 'red '(red shoes blue socks))
;'(red shoes blue socks)