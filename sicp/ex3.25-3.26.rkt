#lang racket
(require racket/mpair)
(require compatibility/mlist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.25 - arbitrary number of keys (not just one or two dim as above)
'(Exercise 3.25)

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (mcar (mcar records))) 
         (mcar records))
        (else (assoc key (mcdr records)))))

;second N-D re-write using lists

(define (make-ND-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup keys)
      (define (iter keys table)
        (let ((subtable 
               (assoc (mcar keys) (mcdr table))))
          (if subtable
              (if (null? (mcdr keys))
                  (mcdr subtable)
                  (iter (mcdr keys) subtable))
              #f)))
      (iter keys local-table))

    (define (insert! list-of-keys value)
      (define (iter keys table)
        (let ((subtable 
               (assoc (mcar keys) (mcdr table))))
          (if subtable
              
              (if (null? (mcdr keys))
                  (set-mcdr! subtable value)
                  (iter (mcdr keys) subtable))
              
              (let ((local-table
                     (set-mcdr! 
                      table
                      (mcons
                       (if (null? (mcdr keys))
                           (mcons (mcar keys) value)
                           (iter (mcdr keys) (mcons (mcar keys) '())))
                       (mcdr table)))))
                table))))
      (iter list-of-keys local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))
 
(define nd-key (make-ND-table))
((nd-key 'insert-proc!) (mlist 'jarek 'car 'make) 'fiat)
((nd-key 'insert-proc!) (mlist 'marta 'car 'make) 'polonez)
((nd-key 'lookup-proc)  (mlist 'jarek 'car 'make))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.26 - see the sketch
'(Exercise 3.26)



