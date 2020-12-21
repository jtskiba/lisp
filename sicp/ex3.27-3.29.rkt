#lang racket
(require racket/mpair)
(require compatibility/mlist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.27
'(Exercise 3.27)

(define (make-table)
  (mlist '*table*))



(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records))) 
         (mcar records))
        (else (assoc key (mcdr records)))))

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                  (mcons (mcons key value) 
                        (mcdr table)))))
  'ok)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result 
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize 
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else 
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))



(memo-fib 3) ;does it in steps proportioal to n, due to nature of memo-fib and how it adds up subseqeunt terms of which there are n
((memoize fib) 3)

;(memo-fib 20000) ; this initially took 20 seconds to calculate but on second attempt it was instanteneous as it returned the result from memory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3.3.4 A Simulator for Digital Circuits
'(3.3.4 A Simulator for Digital Circuits)

(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '()))
    (mcons signal-value action-procedures)))



(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and c s)
  (cond ((and (= c 1) (= s 1)) 1)
        ((or (and (= c 0) (= s 1)) (and (= c 1) (= s 0)) (and (= c 0) (= s 0))) 0)
        (else (error "At least one invalid signal" c s))))

(define (logical-or c s)
  (cond ((and (= c 0) (= s 0)) 0)
        ((or (and (= c 0) (= s 1)) (and (= c 1) (= s 0)) (and (= c 1) (= s 1))) 1)
        (else (error "At least one invalid signal" c s))))

(define (logical-nor c s)
  (cond ((or (and (= c 0) (= s 0)) (and (= c 1) (= s 1))) 0)
        ((or (and (= c 0) (= s 1)) (and (= c 1) (= s 0))) 1)
        (else (error "At least one invalid signal" c s))))

(define (get-signal wire)
  (mcar wire))

(define (set-signal! wire new-value)
  (if (or (= new-value 0) (= new-value 1))
      (if (not (= (get-signal wire) new-value))
          (begin (set-mcar! wire new-value)
                 (mcdr wire))
          "Not a new-value")
      (error "Invalid new-value" new-value)))

(define (add-action! wire procedure-of-no-arguments)
  (set-mcdr! wire (procedure-of-no-arguments)))

;(define (plus1)
;  'haha)

(define (make-agenda) (mlist 0))
(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time)
  (set-mcar! agenda time))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))
(define (first-segment agenda) 
  (mcar (segments agenda)))
(define (rest-segments agenda) 
  (mcdr (segments agenda)))

(define (make-queue) (mcons '() '()))

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) 
  (set-mcar! queue item))
(define (set-rear-ptr! queue item) 
  (set-mcdr! queue item))

(define (empty-queue? queue) 
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an 
              empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-mcdr! (rear-ptr queue) 
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (make-time-segment time queue)
  (mcons time queue))

(define (segment-time s) (mcar s))

(define (segment-queue s) (mcdr s))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time 
           (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! 
         (segment-queue (mcar segments))
         action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr!
               segments
               (mcons (make-new-time-segment 
                      time 
                      action)
                     (mcdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (mcons (make-new-time-segment 
                time 
                action)
               segments))
        (add-to-segments! segments))))

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! 
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define inverter-delay 0)
(define and-gate-delay 3)
(define or-gate-delay 5)


(define (inverter input output)
  (define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
      ;(after-delay 
       ;inverter-delay
       ;(lambda ()
         (set-signal! output new-value))););)
  (add-action! input invert-input)
  'ok)
  


(define W (make-wire))
(define V (make-wire))
(get-signal V)
(inverter W V)
(get-signal V)

;(set-signal! W 1)
;(get-signal W)
;(set-signal! W 1)
;(get-signal W)


;(add-action! ⟨wire⟩ ⟨procedure of no arguments⟩)

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

;or-gate

; (define (and-gate a1 a2 output)

; AND-gate

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.28
'(Exercise 3.28)

; OR-gate

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;Exercise 3.29 - see sketch
'(Exercise 3.29 - see sketch)

;Exercise 3.30 - skipped for now
'(Exercise 3.29 - skipped for now)

