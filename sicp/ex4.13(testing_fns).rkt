#lang sicp
(define (remove1 var frame)
  (define final (car frame))
  (display (car frame))
  (display "\n")
  (cond [(null? frame) #f]
        [(eq? var (caar frame))
         ;(set! frame (cons (cadr frame) (cddr frame)))]
         (if (null? (cdr frame))
             (begin 
               (set! frame final)
               'ok-removed-final)
             (begin
               (set-car! frame (cadr frame))
               (set-cdr! frame (cddr frame))
               'ok-removed))]
        [else (remove1 var (cdr frame))]))

(define (make-unbound var env) ; ADD ex4.13
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (if (bound? var frame)
              (remove1 var frame)
              (env-loop (enclosing-environment env))))))
  (env-loop env))


;(driver-loop)

;;; testing
(define glob the-global-environment)
(define f1 (first-frame glob))


;f1
;(remove 'false f1)
;f1

;(define l (list '(a 1) '(b 2)))
;l

glob
;(make-unbound 'false glob)
glob
'--------------------------
(define lis (list '(a 1) '(b 2)))
(define finale (car lis))
lis

;final
(set! lis finale)
lis