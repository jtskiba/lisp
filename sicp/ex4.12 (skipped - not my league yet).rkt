#lang sicp

;;;;;;;;;;;;;;;;;;INTERNAL DEF's for DATA-DRIVEN PROGRAMMING ;;;;;;;;;;;;;;
; thanks to:
; http://notebook.xyli.me/SICP/SICP-with-DrRacket/

(#%require (only racket/base make-hash hash-ref hash-set!))

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ex4.11 thanks to https://wizardbook.wordpress.com/2010/12/27/exercise-4-11/ )
;; below is the old commented out code and the new one just beneath the old for comparison and learning

(define (met-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first-val (eval (first-operand exps) env)))
        (cons first-val
              (list-of-values-left-to-right (rest-operands exps) env)))))


(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                  
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (if (not (equal? (car (cond-actions first)) '=>))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))
                
                (make-if (cond-predicate first)

                         (list (cadr (cond-actions first))
                                              (cond-predicate first))
                         (expand-clauses rest))
                )))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;(define (make-frame variables values)
;  (cons variables values))
(define (make-frame variables values) ; already in previous ver , now also in solts to ex4.12
  (map cons variables values))

(define (frame-binding var frame) (assoc var frame)) ; ADDDDDDDED ex4.12
(define bound? frame-binding) ; ADDDDDDDED ex4.12


(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

;(define (add-binding-to-frame! var val frame)
;  (set-car! frame (cons var (car frame)))
;  (set-cdr! frame (cons val (cdr frame))))
(define (add-binding-to-frame! var val frame)  ; already in previous ver , now also in solts to ex4.12
  (define (add-binding! binding frame)
    (cond ((null? (cdr frame)) (set-cdr! frame binding))
          (else (add-binding! binding (cdr frame)))))
  (add-binding! (list (cons var val)) frame))

(define (set-binding-in-frame! var val frame) ; ADDDDDDDED ex4.12
  (set-cdr! (frame-binding var frame) val))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


;(define (lookup-variable-value var env)
;  (define (env-loop env)
;    (define (scan frame)
;      (let ((binding (assoc var frame)))
;        (if binding
;            (cdr binding)
;            (env-loop (enclosing-environment env)))))
;    (if (eq? env the-empty-environment)
;        (error "Unbound variable" var)
;        (scan (first-frame env))))
;  (env-loop env))

(define (lookup-variable-value var env) ; ADDDDDDDED ex4.12
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (if (bound? var frame)
              (cdr (frame-binding var frame))
              (env-loop (enclosing-environment env))))))
  (env-loop env))


;(define (set-variable-value! var val env)
;  (define (env-loop env)
;    (define (scan frame)
;      (let ((binding (assoc var frame)))
;        (if binding
;            (set-cdr! binding val)
;            (env-loop (enclosing-environment env)))))
;    (if (eq? env the-empty-environment)
;        (error "Unbound variable -- SET!" var)
;        (scan (first-frame env))))
;  (env-loop env))

(define (set-variable-value! var val env); ADDDDDDDED ex4.12
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (if (bound? var frame)
              (set-binding-in-frame! var val frame)
              (env-loop (enclosing-environment env))))))    
  (env-loop env))


;(define (define-variable! var val env)
;  (let* ((frame (first-frame env))
;         (binding (assoc var frame)))
;    (if binding
;        (set-cdr! binding val)
;        (add-binding-to-frame! var val frame))))
;

(define (define-variable! var val env) ; ADDDDDDDED ex4.12
  (let ((frame (first-frame env)))
    (if (bound? var frame)
        (set-binding-in-frame! var val frame)
        (add-binding-to-frame! var val frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '< <)
        (list '>= >=)
        (list '<= <=)
        (list '= =)
        (list 'display display)
        ;(list 'list list)
        ;<more primitives>
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define apply-in-underlying-scheme apply)

; REPL
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;INSTALATION;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (installation)
  (define (is-lambda? exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
  (define (is-begin? exp env) 
    (eval-sequence (begin-actions exp) env))
  (define (is-cond? exp env) (eval (cond->if exp) env))
   
  (define (eval-and exp env)
    (if (null? (cdr exp))
        #t
        (let ((left (cadr exp)))
          (if (not (eval left env))
              #f
              (if (null? (cddr exp))
                  #t
                  (let ((right (caddr exp)))
                    (if (not (eval right env))
                        #f
                        #t)))))))

  (define (eval-or exp env)
    (if (null? (cdr exp))
        #f
        (let ((left (cadr exp)))
          (if (eval left env)
              #t
              (if (null? (cddr exp))
                  #f
                  (let ((right (caddr exp)))
                    (if (eval right env)
                        #t
                        #f)))))))
  
  (define (let-clauses exp)
    (cdr exp))
  (define (let-assignments clauses)
    (car clauses))
  (define (let-body clauses)
    (cadr clauses))
  (define (first-assignment assignments)
    (car assignments))
  (define (rest-assignments assignments)
    (cdr assignments))
  (define (let-var assignment)
    (car assignment))
  (define (let-exp assignment)
    (cadr assignment))

  (define (assignments->variables assignments)
    (if (null? assignments)
        '()
        (cons (let-var (first-assignment assignments))
              (assignments->variables (rest-assignments assignments)))))

  (define (assignments->expressions assignments)
    (if (null? assignments)
        '()
        (cons (let-exp (first-assignment assignments))
              (assignments->expressions (rest-assignments assignments)))))

  (define (let->lambda exp)
    (let ((params
           (assignments->variables
            (let-assignments (let-clauses  exp))))
          (body-part
           (let-body (let-clauses exp)))
          (applied-to
           (assignments->expressions
            (let-assignments (let-clauses  exp)))))
      (cons (list 'lambda params body-part)
            applied-to)
      ))

  (define (let->combination exp env)
       (met-apply (eval (operator (let->lambda exp)) env)
                  (list-of-values (operands (let->lambda exp)) env)))


  (define (let2-initials exp)
    (if (symbol? (cadr exp))
        (map cadr (caddr exp))
        (map cadr (cadr exp))))
  
  (define (let2-parameters exp) 
    (if (symbol? (cadr exp))
        (map car (caddr exp))
        (map car (cadr exp))))
  
  (define (let2-body exp) 
    (if (symbol? (cadr exp))
        (cdddr exp)
        (cddr exp)))


  (define (is-let? exp) 
    (cons (make-lambda (let2-parameters exp) 
                       (let2-body exp))
          (let2-initials exp)))

  (define (is-let2? exp) 
    (if (symbol? (cadr exp))
        (list 'let
              (caddr exp)
              (list
               'define (cons (cadr exp) (let2-parameters exp)) 
               (car (let2-body exp)))
              (cons (cadr exp) (let2-parameters exp)))
        (cons (make-lambda (let2-parameters exp) 
                           (let2-body exp))
              (let2-initials exp))))
 
  (define (eval-let exp env)
    (eval (is-let2? exp) env)) 

  (define (let*->nested-lets initials parameters body)
    (define (iter initials parameters body)
      (list
       'let 
       (list (list (car parameters) (car initials)))
       (if (null? (cdr parameters))
           (car body)
           (iter (cdr initials) (cdr parameters) body))))
    (iter initials parameters body))

  (define (eval-let* exp env)
    (eval (let*->nested-lets (let2-initials exp)
                             (let2-parameters exp)
                             (let2-body exp))
          env))

  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda is-lambda?)
  (put 'eval 'begin is-begin?)
  (put 'eval 'cond is-cond?)
  (put 'eval 'and eval-and)
  (put 'eval 'or eval-or)
  (put 'eval 'let1 let->combination)
  (put 'eval 'let eval-let)
  (put 'eval 'let* eval-let*)
  )

(installation)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp) (text-of-quotation exp))
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (operator exp))                
         ((get 'eval (operator exp)) 
               exp env))
        ((application? exp)
         (met-apply (eval (operator exp) env)
                    (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(driver-loop)

