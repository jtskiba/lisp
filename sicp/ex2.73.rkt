#lang sicp
;from sicp 2.4 - complex number rectangular (add/subt) and polar (mult/div) representations to use for various operations
(define (square a) (* a a))
;
;;(make-from-real-imag (real-part z) 
;;                     (imag-part z))
;
;;(make-from-mag-ang (magnitude z) 
;;                   (angle z))
;
;(define (add-complex z1 z2)
;  (make-from-real-imag 
;   (+ (real-part z1) (real-part z2))
;   (+ (imag-part z1) (imag-part z2))))
;
;(define (sub-complex z1 z2)
;  (make-from-real-imag 
;   (- (real-part z1) (real-part z2))
;   (- (imag-part z1) (imag-part z2))))
;
;(define (mul-complex z1 z2)
;  (make-from-mag-ang 
;   (* (magnitude z1) (magnitude z2))
;   (+ (angle z1) (angle z2))))
;
;(define (div-complex z1 z2)
;  (make-from-mag-ang 
;   (/ (magnitude z1) (magnitude z2))
;   (- (angle z1) (angle z2))))


;ben's rectangular representation
;(define (real-part z) (car z))
;(define (imag-part z) (cdr z))
;
;(define (magnitude z)
;  (sqrt (+ (square (real-part z)) 
;           (square (imag-part z)))))
;
;(define (angle z)
;  (atan (imag-part z) (real-part z))) ;returns an angle such that tan of that angle is equal to im-part y / re-part x
;
;(define (make-from-real-imag x y) 
;  (cons x y))
;
;(define (make-from-mag-ang r a)
;  (cons (* r (cos a)) (* r (sin a))))

;alyssa's polar representation
;(define (real-part z)
;  (* (magnitude z) (cos (angle z))))
;
;(define (imag-part z)
;  (* (magnitude z) (sin (angle z))))
;
;(define (magnitude z) (car z))
;
;(define (angle z) (cdr z))
;
;(define (make-from-real-imag x y)
;  (cons (sqrt (+ (square x) (square y)))
;        (atan y x)))
;
;(define (make-from-mag-ang r a) 
;  (cons r a))

;show how it works
;starting from rectang representation of ben (comment out alyssas) x=2, y=3

;(define z1 (make-from-real-imag  2 3))
;z1
;(add-complex z1 z1)
;(sub-complex z1 z1)
;(mul-complex z1 z1)
;(div-complex z1 z1)
;(magnitude z1)
;(angle z1)
;results
;'(4 . 6)
;'(0 . 0)
;'(-4.999999999999999 . 11.999999999999998)
;'(1.0 . 0.0)
;3.605551275463989
;0.982793723247329

;moving to polar representation of alyssa (comment out bens) r=3.605551275463989, a=0.982793723247329

;(define z1 (make-from-real-imag  2 3))
;z1
;(add-complex z1 z1)
;(sub-complex z1 z1)
;(mul-complex z1 z1)
;(div-complex z1 z1)
;(magnitude z1)
;(angle z1)
;;results
;'(3.605551275463989 . 0.982793723247329)
;'(7.211102550927978 . 0.982793723247329)
;'(0.0 . 0.0)
;'(12.999999999999998 . 1.965587446494658)
;'(1.0 . 0.0)
;3.605551275463989
;0.982793723247329

;Tagged-data SICP 2.4.2
; this is to add types to cx number - whether it is rectangular or polar representation
;(define (attach-tag type-tag contents)
;  (cons type-tag contents))

;extract tag type
;(define (type-tag datum)
;  (if (pair? datum)
;      (car datum)
;      (error "Bad tagged datum: 
;              TYPE-TAG" datum)))
;;extract actual cx number
;(define (contents datum)
;  (if (pair? datum)
;      (cdr datum)
;      (error "Bad tagged datum: 
;              CONTENTS" datum)))

;check if cx number is in polar or rectangular representation
;(define (rectangular? z)
;  (eq? (type-tag z) 'rectangular))
;
;(define (polar? z)
;  (eq? (type-tag z) 'polar))

;  Ben’s revised rectangular representation:  - same just added rectngular to fn names
;(define (real-part-rectangular z) (car z))
;(define (imag-part-rectangular z) (cdr z))
;
;(define (magnitude-rectangular z)
;  (sqrt (+ (square (real-part-rectangular z))
;           (square (imag-part-rectangular z)))))
;
;(define (angle-rectangular z)
;  (atan (imag-part-rectangular z)
;        (real-part-rectangular z)))
;
;(define (make-from-real-imag-rectangular x y)
;  (attach-tag 'rectangular (cons x y)))
;
;(define (make-from-mag-ang-rectangular r a)
;  (attach-tag 
;   'rectangular
;   (cons (* r (cos a)) (* r (sin a)))))

;Alyssa’s revised polar representation: - same just added polar to fn names
;(define (real-part-polar z)
;  (* (magnitude-polar z) 
;     (cos (angle-polar z))))
;
;(define (imag-part-polar z)
;  (* (magnitude-polar z) 
;     (sin (angle-polar z))))
;
;(define (magnitude-polar z) (car z))
;
;(define (angle-polar z) (cdr z))
;
;(define (make-from-real-imag-polar x y)
;  (attach-tag 
;   'polar
;   (cons (sqrt (+ (square x) (square y)))
;         (atan y x))))
;
;(define (make-from-mag-ang-polar r a)
;  (attach-tag 'polar (cons r a)))

; now how to use fns that switch between the two - these are to select real/imag part or magn/angle from any polar/rectang representation

;(define (real-part z)
;  (cond ((rectangular? z)
;         (real-part-rectangular (contents z)))
;        ((polar? z)
;         (real-part-polar (contents z)))
;        (else (error "Unknown type: 
;               REAL-PART" z))))
;
;(define (imag-part z)
;  (cond ((rectangular? z)
;         (imag-part-rectangular (contents z)))
;        ((polar? z)
;         (imag-part-polar (contents z)))
;        (else (error "Unknown type: 
;               IMAG-PART" z))))
;
;(define (magnitude z)
;  (cond ((rectangular? z)
;         (magnitude-rectangular (contents z)))
;        ((polar? z)
;         (magnitude-polar (contents z)))
;        (else (error "Unknown type: 
;               MAGNITUDE" z))))
;
;(define (angle z)
;  (cond ((rectangular? z)
;         (angle-rectangular (contents z)))
;        ((polar? z)
;         (angle-polar (contents z)))
;        (else (error "Unknown type: 
;               ANGLE" z))))

; operations stay the same because they are generic
;(define (add-complex z1 z2)
;  (make-from-real-imag 
;   (+ (real-part z1) (real-part z2))
;   (+ (imag-part z1) (imag-part z2))))
;
;(define (sub-complex z1 z2)
;  (make-from-real-imag 
;   (- (real-part z1) (real-part z2))
;   (- (imag-part z1) (imag-part z2))))
;
;(define (mul-complex z1 z2)
;  (make-from-mag-ang 
;   (* (magnitude z1) (magnitude z2))
;   (+ (angle z1) (angle z2))))
;
;(define (div-complex z1 z2)
;  (make-from-mag-ang 
;   (/ (magnitude z1) (magnitude z2))
;   (- (angle z1) (angle z2))))

; need the below to make operation work - this is so that
; - if you have x and y coord, you pick rectang representation, AND
; - if polar magn and angle, you pick polar representation.

;(define (make-from-real-imag x y)
;  (make-from-real-imag-rectangular x y))
;
;(define (make-from-mag-ang r a)
;  (make-from-mag-ang-polar r a))



;    Data-Directed Programming and Additivity SICP 2.4.3
; idea is to have a table whih defines procedure for a given operation and type, (using PUT)
; and aslo GET which gets a relevant procedures when presented with oper and type

; this is to add types to cx number - whether it is rectangular or polar representation [SAME AS ABOVE]
;(define (attach-tag type-tag contents)
;  (cons type-tag contents))
;
;
;; ben's package
;
;(define (install-rectangular-package)
;  ;; internal procedures
;  (define (real-part z) (car z))
;  (define (imag-part z) (cdr z))
;  (define (make-from-real-imag x y) 
;    (cons x y))
;  (define (magnitude z)
;    (sqrt (+ (square (real-part z))
;             (square (imag-part z)))))
;  (define (angle z)
;    (atan (imag-part z) (real-part z)))
;  (define (make-from-mag-ang r a)
;    (cons (* r (cos a)) (* r (sin a))))
;  ;; interface to the rest of the system
;  (define (tag x) 
;    (attach-tag 'rectangular x))
;  (put 'real-part '(rectangular) real-part)
;  (put 'imag-part '(rectangular) imag-part)
;  (put 'magnitude '(rectangular) magnitude)
;  (put 'angle '(rectangular) angle)
;  (put 'make-from-real-imag 'rectangular
;       (lambda (x y) 
;         (tag (make-from-real-imag x y))))
;  (put 'make-from-mag-ang 'rectangular
;       (lambda (r a) 
;         (tag (make-from-mag-ang r a))))
;  'done)
;
;;alyssa's package:
;
;(define (install-polar-package)
;  ;; internal procedures
;  (define (magnitude z) (car z))
;  (define (angle z) (cdr z))
;  (define (make-from-mag-ang r a) (cons r a))
;  (define (real-part z)
;    (* (magnitude z) (cos (angle z))))
;  (define (imag-part z)
;    (* (magnitude z) (sin (angle z))))
;  (define (make-from-real-imag x y)
;    (cons (sqrt (+ (square x) (square y)))
;          (atan y x)))
;  ;; interface to the rest of the system
;  (define (tag x) (attach-tag 'polar x))
;  (put 'real-part '(polar) real-part)
;  (put 'imag-part '(polar) imag-part)
;  (put 'magnitude '(polar) magnitude)
;  (put 'angle '(polar) angle)
;  (put 'make-from-real-imag 'polar
;       (lambda (x y)s 
;         (tag (make-from-real-imag x y))))
;  (put 'make-from-mag-ang 'polar
;       (lambda (r a) 
;         (tag (make-from-mag-ang r a))))
;  'done)
;
;; Apply-generic looks in the table under the name of the operation and
;; the types of the arguments and applies the resulting procedure if one is present:
;
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error
;            "No method for these types: 
;             APPLY-GENERIC"
;            (list op type-tags))))))
;
;; define our generic selectors as follows: 
;(define (real-part z) 
;  (apply-generic 'real-part z))
;(define (imag-part z) 
;  (apply-generic 'imag-part z))
;(define (magnitude z) 
;  (apply-generic 'magnitude z))
;(define (angle z) 
;  (apply-generic 'angle z))
;
;
;(define (make-from-real-imag x y)
;  ((get 'make-from-real-imag 
;        'rectangular) 
;   x y))
;
;(define (make-from-mag-ang r a)
;  ((get 'make-from-mag-ang 
;        'polar) 
;   r a))


;;;;; EX 2.73
;to get put and get - will look into in later chapters - here assumed works
;(define (lookup key table)
;  (let ((record (assoc key (cdr table))))
;    (if record
;        (cdr record)
;        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

 ;derivative
(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv2 (addend exp) var)
                   (deriv2 (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product 
            (multiplier exp)
            (deriv2 (multiplicand exp) var))
           (make-product 
            (deriv2 (multiplier exp) var)
            (multiplicand exp))))
        ;⟨more rules can be added here⟩
        (else (error "unknown expression type:
                      DERIV2" exp))))



;(define (insert! key value table)
;  (let ((record (assoc key (cdr table))))
;    (if record
;        (set-cdr! record value)
;        (set-cdr! table
;                  (cons (cons key value) 
;                        (cdr table)))))
;  'ok)
;
;(define (make-table)
;  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record 
               (assoc key-2 (cdr subtable))))
          (if record (cdr record) false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record 
               (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! 
               subtable
               (cons (cons key-2 value)
                     (cdr subtable)))))
        (set-cdr! 
         table
         (cons (list key-1 (cons key-2 value))
               (cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;;;;;;;;;;;;;;;;;




(define (deriv-sum exp var)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))

(define (deriv-product exp var)
  (make-sum (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))

(define (install-deriv)
  (put ('deriv '+ deriv-sum))
  (put ('deriv '* deriv-product))
  'done)


(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))


(define (operator exp) (car exp))
(define (operands exp) (cdr exp))



;(deriv '(* x (+ x 3)) 'x)
(deriv2 '(* x y) 'x)
(deriv '(* x y) 'x)