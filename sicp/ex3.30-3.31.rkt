#lang racket
(require racket/mpair)
(require compatibility/mlist)

; queue
'(Representing Queues)

(define (front-ptr queue) (mcar queue))

(define (rear-ptr queue) (mcdr queue))

(define (set-front-ptr! queue item) 
  (set-mcar! queue item))

(define (set-rear-ptr! queue item) 
  (set-mcdr! queue item))

(define (empty-queue? queue) 
  (null? (front-ptr queue)))

(define (make-queue) (mcons '() '()))

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

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with 
                 an empty queue" queue))
        (else (set-front-ptr! 
               queue 
               (mcdr (front-ptr queue)))
              queue)))

;Representing wires
(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each 
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures 
           (mcons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) 
             signal-value)
            ((eq? m 'set-signal!) 
             set-my-signal!)
            ((eq? m 'add-action!) 
             accept-action-procedure!)
            (else (error "Unknown operation: 
                          WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((mcar procedures))
             (call-each (mcdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

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

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (make-time-segment time queue)
  (mcons time queue))
(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: 
              FIRST-AGENDA-ITEM")
      (let ((first-seg 
             (first-segment agenda)))
        (set-current-time! 
         agenda 
         (segment-time first-seg))
        (front-queue 
         (segment-queue first-seg)))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue 
            (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! 
         agenda 
         (rest-segments agenda))
        '())))

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

(define (after-delay delay action)
  (add-to-agenda! 
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item 
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

; A sample simulation

(define (inverter input output)
  (define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
      (after-delay 
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

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

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    ;(display "here " )
    ;(display (get-signal c))
    (inverter c e)
    ;(display "there " )
    ;(display (get-signal e))
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))



;;;;;

(define (probe name wire)
  (add-action! 
   wire
   (lambda ()
     (newline)
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display "  New-value = ")
     (display (get-signal wire))
     )))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;Implementing the agenda

(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
(set-signal! input-2 1)
(propagate)
'()

;Ex3.30
'(Ex 3.30i)

(define a (make-wire))
(set-signal! a 0)
(define b (make-wire))
(set-signal! b 0)
(define c-in (make-wire))
(set-signal! c-in 1)
(define suma (make-wire))
(define c-out (make-wire))
(define d (make-wire))

(and-gate a b d)
(propagate)
(get-signal d)


(half-adder a b suma c-out)
(propagate)
(get-signal suma)
(get-signal c-out)

(full-adder a b c-in suma c-out)
(propagate)
(get-signal suma)
(get-signal c-out)

(define (ripple-carry-adder list-A list-B list-S)
  (let ((c-in (make-wire)))
    (define (iter a b cin s)
      (let ((cout (make-wire)))            
        (full-adder (mcar a) (mcar b) cin (mcar s) cout)
        (propagate)
        (if (null? (mcdr a))
            'done-for-good
            (iter (mcdr a) (mcdr b) cout (mcdr s)))))
    (iter list-A list-B c-in list-S)))

(define a1 (make-wire))
(define a2 (make-wire))
(define a3 (make-wire))
(set-signal! a1 1)
(set-signal! a2 1)
(define LIST-A (mlist a1 a2 a3))

(define b1 (make-wire))
(define b2 (make-wire))
(define b3 (make-wire))
(set-signal! b1 1)
(set-signal! b2 1)
(define LIST-B (mlist b1 b2 b3))

(define s1 (make-wire))
(define s2 (make-wire))
(define s3 (make-wire))
(define LIST-S (mlist s1 s2 s3))

(ripple-carry-adder LIST-A LIST-B LIST-S)

(get-signal (mcar LIST-S))
(get-signal (mcar (mcdr LIST-S)))
(get-signal (mcar (mcdr (mcdr LIST-S))))

;Ex3.31
'(Ex 3.31)

; Without executing the procedure immediately the system would never actually start – nothing would be added to the agenda, no actions would occur and no signals would propagate.
; https://wizardbook.wordpress.com/2010/12/17/exercise-3-31/

;Ex3.32
'(Ex 3.32)
; my sketch of full adder in pict
; https://wizardbook.wordpress.com/2010/12/17/exercise-3-32/
;The operations of several of the building blocks are dependent on the results of other operations. Looking back at the half-adder,
; the carry signal C depends on the result of the and-gate. We have carefully built or procedural abstractions to be aware of these dependencies
; but the unstated assumption is that the scheduling of events follow the arrow of time. As we add actions to wires, the underlying agenda items are
; created in the order in which they must occur. A queue implemented as LIFO (like a stack) would reverse the order of operations.
; Time would run backwards in the simulation. A FIFO queue works well, as would a priority queue.


