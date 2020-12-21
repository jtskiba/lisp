   
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

(define get-coercion (operation-table 'lookup-proc))
(define put-coercion (operation-table 'insert-proc!))


;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;; 	  (apply proc (map contents args))
;;           (error
;; 	   "No method for these types: 
;;         APPLY-GENERIC"
;; 	   (list op type-tags))))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))

;;;;;;;;;;;;
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

;;;;;;;;;;;;;;;;



(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'exp    ;; only for ex2.81
     '(scheme-number scheme-number)
     (lambda (x y) 
       (tag (expt x y)))) 
       ; using primitive expt
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
			  (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (and (= (numer x) 0)
			(not (= (denom x) 0)))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)


(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  (define (magnitude z) (apply-generic 'magnitude z)) 
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y))
			  (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x) (and (= (real-part x) 0)
			(= (imag-part x) 0))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part) ;added
  (put 'imag-part '(complex) imag-part) ;added
  (put 'magnitude '(complex) magnitude) ;added
  (put 'angle '(complex) angle)         ;added 
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; additional generic number selectors
(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z)) 

;; 2.79 and 2.80

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

;; SICP on coercion


(define (install-coercion-package)
  ;; internal procedures
  (define (scheme-number->complex n)
    (make-complex-from-real-imag 
     (contents n) 0))
  (define (scheme-number->scheme-number n) n) ;for ex2.81only
  (define (complex->complex z) z)  ;for ex2.81only
  
  ;; interface to rest of the system
  (put-coercion 'scheme-number 'complex 
		scheme-number->complex)
  (put-coercion 'scheme-number 'scheme-number   ;for ex2.81only
              scheme-number->scheme-number)
  (put-coercion 'complex 'complex    ;for ex2.81only
		complex->complex)
  'done)

;; install

(install-complex-package)
(install-polar-package)
(install-rectangular-package)
(install-rational-package)
(install-scheme-number-package)
(install-coercion-package)


;; evaluation

(define z1 (make-complex-from-real-imag 3 4))
(define z2 (make-complex-from-real-imag 3 2))
(define z3 (make-complex-from-mag-ang 2 3))
(define z4 (make-complex-from-real-imag 2 4))
(define z5 (make-complex-from-real-imag 0 0))
(define z6 (make-complex-from-real-imag 0 1))
(define z7 (make-complex-from-real-imag 1 0))
(define z8 (make-complex-from-real-imag 2 0))
(define z9 (make-complex-from-real-imag 3 0))



(define a1 (make-scheme-number 2))
(define a2 (make-scheme-number 2))
(define a3 (make-scheme-number 0))
(define a4 (make-scheme-number 3))


(define b1 (make-rational 1 2))
(define b2 (make-rational 1 4))
(define b3 (make-rational 2 4))
(define b4 (make-rational 0 2))


(define c1 (list 2 3))


;; (equ? a1 a1)
;; (equ? b1 b3)
;; (equ? z1 z1)
;; (equ? z3 z3)
;; (equ? z3 z1)
;;
;; (magnitude z1)
;; (add z1 z2)

;; (=zero? a3)
;; (=zero? b4)
;; (=zero? z5)

;; (=zero? a2)
;; (=zero? b3)
;; (=zero? z6)
;; (=zero? z7)

(add a1 a1)
(add z1 z1)

(add a1 z1)
(add z1 a1)
(exp a1 a4)

;;(exp z8 z9)
;; 2.81: should be 8 but we are working with cx numbers
;; previously we would get an error because no exp defined in cx package
;; then it would try to coerse types so they are aligned but apply-generic
;; is not able to coerse complex to complex so would be an error
;; however, in situation where we added cx to cx type coercion we no longer
;; get that error, instead we coerse from cx to cx, and loop to apply-gen
;; again, but this time we dont find exp in cx package either hence we hve
;; an infinite loop now....

;; point 2 of qn explained already,we dont need to coerse to same
;; type. current apply-generic deals with it ok, because if types are
;; the same, then the followin does it, if not found, then will move to ifs
;;           (apply proc (map contents args))

;; having said that, JoTings raises good point that if we keep the
;; apply-generic as is, it will have some inefficiency. as in, once it
;; cant find the procedure, it will go to coerse and still try coercing on
;; same types but we know this is going to give us nothing.
;; hence he creates and additional if statement before coercion begins to
;; check if we have two same types and if we do, don't proceed any further
;; JoTTings verson of apply-generic below

;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (if (= (length args) 2)
;;               (let ((type1 (car type-tags))
;;                     (type2 (cadr type-tags)))
;;                 (if (not (eq? type1 type2))
;;                     (let ((t1->t2 (get-coercion type1 type2))
;;                           (t2->t1 (get-coercion type2 type1))
;;                           (a1 (car args))
;;                           (a2 (cadr args)))
;;                       (cond (t1->t2
;;                              (apply-generic op (t1->t2 a1) a2))
;;                             (t2->t1
;;                              (apply-generic op a1 (t2->t1 a2)))
;;                             (else
;;                              (error "No method for these types"
;;                                     (list op type-tags)))))
;;                     (error "No method for these types"
;;                                     (list op type-tags))))
;;               (error "No method for these types"
;;                      (list op type-tags)))))))
