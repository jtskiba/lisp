#lang racket
(require racket/mpair)
(require compatibility/mlist)

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector 
                    new-value 
                    informant)
  ((connector 'set-value!) 
   new-value 
   informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception 
                         procedure 
                         list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) 
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (make-connector)
  (let ((value false) 
        (informant false) 
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except 
              setter
              inform-about-value
              constraints))
            ((not (= value newval))
             (error "Contradiction" 
                    (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except 
                  retractor
                  inform-about-no-value
                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint 
                     constraints))
          (set! constraints
                (cons new-constraint 
                      constraints))
          'ok)
      (if (has-value? me)
          (inform-about-value new-constraint)
          'ok)
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) 
             set-my-value)
            ((eq? request 'forget) 
             forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: 
                          CONNECTOR"
                         request))))
    me))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) 
                (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) 
                          (get-value a2))
                       me))
          ((and (has-value? a1) 
                (has-value? sum))
           (set-value! a2
                       (- (get-value sum) 
                          (get-value a1))
                       me))
          ((and (has-value? a2) 
                (has-value? sum))
           (set-value! a1
                       (- (get-value sum) 
                          (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) 
                    (= (get-value m1) 0))
               (and (has-value? m2) 
                    (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) 
                (has-value? m2))
           (set-value! product
                       (* (get-value m1) 
                          (get-value m2))
                       me))
          ((and (has-value? product) 
                (has-value? m1))
           (set-value! m2
                       (/ (get-value product) 
                          (get-value m1))
                       me))
          ((and (has-value? product) 
                (has-value? m2))
           (set-value! m1
                       (/ (get-value product) 
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request: 
                   MULTIPLIER" 
                  request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" 
           request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ")
    (display name) (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: 
                        PROBE" request))))
  (connect connector me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))


(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 212 'user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
'(Ex3.33)
;Ex 3.33

(define (averager a b c)
  (let ((u (make-connector))
        (x (make-connector)))
    (adder a b u)
    (multiplier c x u)
    (constant 2 x)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)

(probe "Value of a" a)
(probe "Value of b" b)
(probe "Average value" c)

(set-value! a 2 'user)
(set-value! c 4 'user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
'(Ex3.34)
;Ex 3.34

(define (squarer a1 b1) (multiplier a1 a1 b1))

(define a1 (make-connector))
(define b1 (make-connector))

(squarer a1 b1)

(probe "Value of a1" a1)
(probe "Value of b1" b1)

(set-value! b1 4 'user)
; problem is that our multiplier routine does not have a situation defined whereby both m1 and m2 are the same and without value. We would need to add a conditon to say that
; if it has product and both m1 and m2 have no value then, essentially set both m1 and m2 to the square root of the product.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
'(Ex3.35)
;Ex 3.35

(define (squarer2 a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: 
                    SQUARER" 
                   (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
    (set-value! b
                (* (get-value a) (get-value a))
                me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request: 
                   SQUARER2" 
                  request))))
  (connect a me)
  (connect b me)
  me)

(define (my-squarer a2 b2) (squarer2 a2 b2))

(define a2 (make-connector))
(define b2 (make-connector))

(my-squarer a2 b2)

(probe "Value of a2" a2)
(probe "Value of b2" b2)

(set-value! b2 4 'user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
'(Ex3.36 - skipped, read only for now)
;Ex 3.36
; https://wizardbook.wordpress.com/2010/12/17/exercise-3-36/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
'(Ex3.37)
;Ex 3.37

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z1 (make-connector))
        (z2 (make-connector)))
    (adder x (multiplier (cv -1) y z1) z2)
    z2))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (celsius-fahrenheit-converter2 x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define CC (make-connector))
(define FF (celsius-fahrenheit-converter2 CC))

(celsius-fahrenheit-converter2 CC)

(probe "Value of CC" CC)
(probe "Value of FF" FF)

(set-value! CC 30 'user)

