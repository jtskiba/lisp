 
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



;;;;;;;;;;;;



;cx numbers from sicp 2.4

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


;; (define (install-rectangular-package)
;;   ;; internal procedures
;;   (define (real-part z) (car z))
;;   (define (imag-part z) (cdr z))
;;   (define (make-from-real-imag x y) 
;;     (cons x y))
;;   (define (magnitude z)
;;     (sqrt (+ (square (real-part z))
;;              (square (imag-part z)))))
;;   (define (angle z)
;;     (atan (imag-part z) (real-part z)))
;;   (define (make-from-mag-ang r a)
;;     (cons (* r (cos a)) (* r (sin a))))
;;   ;; interface to the rest of the system
;;   (define (tag x) 
;;     (attach-tag 'rectangular x))
;;   (put 'real-part '(rectangular) real-part)
;;   (put 'imag-part '(rectangular) imag-part)
;;   (put 'magnitude '(rectangular) magnitude)
;;   (put 'angle '(rectangular) angle)
;;   (put 'make-from-real-imag 'rectangular
;;        (lambda (x y) 
;;          (tag (make-from-real-imag x y))))
;;   (put 'make-from-mag-ang 'rectangular
;;        (lambda (r a) 
;;          (tag (make-from-mag-ang r a))))
;;   'done)

;; (define (install-polar-package)
;;   ;; internal procedures
;;   (define (magnitude z) (car z))
;;   (define (angle z) (cdr z))
;;   (define (make-from-mag-ang r a) (cons r a))
;;   (define (real-part z)
;;     (* (magnitude z) (cos (angle z))))
;;   (define (imag-part z)
;;     (* (magnitude z) (sin (angle z))))
;;   (define (make-from-real-imag x y)
;;     (cons (sqrt (+ (square x) (square y)))
;;           (atan y x)))
;;   ;; interface to the rest of the system
;;   (define (tag x) (attach-tag 'polar x))
;;   (put 'real-part '(polar) real-part)
;;   (put 'imag-part '(polar) imag-part)
;;   (put 'magnitude '(polar) magnitude)
;;   (put 'angle '(polar) angle)
;;   (put 'make-from-real-imag 'polar
;;        (lambda (x y) 
;;          (tag (make-from-real-imag x y))))
;;   (put 'make-from-mag-ang 'polar
;;        (lambda (r a) 
;;          (tag (make-from-mag-ang r a))))
;;   'done)

;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;; 	  (apply proc (map contents args))
;;           (error
;; 	   "No method for these types: 
;;         APPLY-GENERIC"
;; 	   (list op type-tags))))))

(define (apply-generic op arg) (arg op))


;; (define (real-part z) 
;;   (apply-generic 'real-part z))
;; (define (imag-part z) 
;;   (apply-generic 'imag-part z))
;; (define (magnitude z) 
;;   (apply-generic 'magnitude z))
;; (define (angle z) 
;;   (apply-generic 'angle z))

;; (define (make-from-real-imag x y)
;;   ((get 'make-from-real-imag 
;;         'rectangular) 
;;    x y))


;; (define (make-from-mag-ang r a)
;;   ((get 'make-from-mag-ang 
;;         'polar) 
;;    r a))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r a) 
  (define (dispatch op) 
    (cond ((eq? op 'real-part) (* r (cos a))) 
	  ((eq? op 'imag-part) (* r (sin a))) 
	  ((eq? op 'magnitude) r) 
	  ((eq? op 'angle) a) 
	  (else (error "Unkown op --- MAKE-FROM-MAG-ANG" op)))) 
  dispatch) 



;; (install-polar-package)
;; (install-rectangular-package)


;;; evaluation


(define pi 3.141592653589793)
(define (degrees->radians d) (/ (* d pi) 180))


(define a (make-from-mag-ang 2 (degrees->radians 45)))
(define b (make-from-mag-ang 3 0))
(define c (make-from-mag-ang 2 (degrees->radians 135)))

(magnitude a)
(angle a)
(real-part a)
(imag-part a)
(real-part b)
(imag-part b)
(real-part c)
(imag-part c)


