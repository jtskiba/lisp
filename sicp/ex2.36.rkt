#lang racket

;ex2.36
(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda(x) (list-ref x 0)) seqs))
            (accumulate-n op init (map (lambda(x) (cdr x)) seqs)))))


(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
s
(accumulate + 0 (map (lambda(x) (list-ref x 0)) s))
(define s1 (map (lambda(x) (cdr x)) s))
s1
(accumulate + 0 (map (lambda(x) (list-ref x 0)) s1))
(define s2 (map (lambda(x) (cdr x)) s1))
s2
(accumulate + 0 (map (lambda(x) (list-ref x 0)) s2))
(define s3 (map (lambda(x) (cdr x)) s2))
s3
(newline)

;(accumulate + 0 (map (lambda(x) (list-ref x 0)) s1))
;(map (lambda(x) (cdr x)) s)
;(accumulate + 0 (map (lambda(x) (list-ref x 0)) s))
;(map (lambda(x)(cdr x)) s)

(accumulate-n + 0 s)
;(22 26 30)





