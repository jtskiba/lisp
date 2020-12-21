#lang racket
(require racket/mpair)
(require compatibility/mlist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This is in relation to chapter 3.3.1
; renamed to mcons, mcar, mcdr, set-mcar!, set-mcdr!, mlist

(define x (mlist (mlist 'a 'b) 'c 'd))
(define y (mlist 'e 'f))
x
y
(set-mcar! x y)
x
(set-mcdr! x y)
x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.12
'(Exercise 3.12)

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(append (list 2 3 4) (list 5 6 7))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(last-pair (list 2 3 4))

(define (last-mpair x)
  (if (null? (mcdr x))
      x
      (last-mpair (mcdr x))))

(define (append! x y)
  (set-mcdr! (last-mpair x) y)
  x)

(append! (mlist 2 3 4) (mlist 5 6 7))

;Consider the interaction
(define x1 (list 'a 'b))
(define y1 (list 'c 'd))
(define z1 (append x1 y1)) ;append does NOT modify x1
z1
(cdr x1)
(cdr z1)

(define x2 (mlist 'a 'b))
(define y2 (mlist 'c 'd))
(define z2 (append! x2 y2)) ;append! modifies x2 already
z2
(mcdr z2)
(mcdr x2)
x2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.13
'(Exercise 3.13)

(define (make-cycle x)
  (set-mcdr! (last-mpair x) x)
  x)
; note-because it is modifying x as it goes, the x never ends, and you finish one cycle, and you end up with 123123 and then append again...

;(make-cycle (mlist 'a 'b 'c))
(define z (make-cycle (mlist 'a 'b 'c)))
z
;(last-mpair z)
;hence the above is (last-mpair z) is an inifinte loop!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.14
'(Exercise 3.14)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))
; this reverses the x, by muting! so x changes when subject to mystery

(define v (mlist 'a 'b 'c 'd))
v
(define w (mystery v))
w
v
;my feel is that both w and v will be the same and reverses of the original v defined as (a b c d), i.e. (d c b a)
; OK it reverses, but v has been modified such that (a '()) is left
; i made a stetch separately to show that the last assignment is (d 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Relates to sharing and identity (3.2)
'(Relates to sharing and identity 3.2)

(define xx (list 'a 'b))
(define z11 (cons xx xx))
(define z22 
  (cons (list 'a 'b) (list 'a 'b)))

z11
z22

;lets define using mutable lists

(define mx (mlist 'a 'b))
(define mz1 (mcons mx mx))
(define mz2 
  (mcons (mlist 'a 'b) (mlist 'a 'b)))

mz1
mz2

(define (set-to-wow! x)
  (set-mcar! (mcar x) 'wow)
  x)

(set-to-wow! mz1)
(set-to-wow! mz2)

;detecting that two symbols are equal (checking if they are using same pointers)
(eq? (mcar mz1) (mcdr mz1))
(eq? (mcar mz2) (mcdr mz2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.15 - skipped, understood
'(Exercise 3.15 - skipped)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.16
'(Exercise 3.16)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define f1 (list 2 3 4 5))
(count-pairs f1) ; here it works

(define f2 (list (cons 1 2) (cons 1 2) (cons 1 2)))
(count-pairs f2) ; this is 6


(define f3 (list 1 2 3))
(count-pairs f3) ; this is 3

(define f4 (list (cons 1 2) (cons 1 2) 1))
(count-pairs f4) ; this is 5

(define f5 (list (cons 1 2) 1 1))
(count-pairs f5) ; this is 4

(define f6 (list (cons 1 2) (list (cons 1 2) (cons 1 2))))
(count-pairs f6) ; this is 7

(define test1 (list 1 2))
(define test2 (cons 1 2))
(count-pairs test1) ;=== this is 2
(count-pairs test2) ;=== this is 1

;lets play with assignments
(define (count-mpairs x)
  (if (not (mpair? x))
      0
      (+ (count-mpairs (mcar x))
         (count-mpairs (mcdr x))
         1)))

(define my-c (mcons 1 2))
(define my-l1 (mlist my-c my-c my-c))
my-l1
(count-mpairs my-l1)

;used examples from wizardbook.wordpress (https://wizardbook.wordpress.com/2010/12/15/exercise-3-16/)
'(examples from wizard book for ex 3.16)

(define L31 (mlist 'a 'b 'c))
 
(define L41 (mlist 'b 'c))
(define L42 (mlist 'a))
(set-mcar! L41 L42)
(set-mcar! (mcdr L41) L42)

(count-mpairs L31) ; thats our 3
(count-mpairs L41) ; thats our 4

(define L71 (mlist 'c))
(define L72 (mlist 'b))
(define L73 (mlist 'a))
(set-mcar! L72 L73)
(set-mcdr! L72 L73)
(set-mcar! L71 L72)
(set-mcdr! L71 L72)

(count-mpairs L71) ; thats our 7
; not sure if this is done correctly either

(define linf (mlist 'a 'b 'c))
(set-mcdr! (mcdr (mcdr linf)) linf) ; creates an infinite cycle

;(count-mpairs linf) ; thats inifinite result


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.17
'(Exercise 3.17)
;stoled from below for analysis:
; source: https://wizardbook.wordpress.com/2010/12/15/exercise-3-17/


(define e1 (mlist 1 2))
(define e2 (mlist 1 2))
(define e3 (mlist 1 2))
(define e4 (mlist 1 2))
(define e5 (mlist 3 4))

e1
e2
(set-mcar! e1 e2)
(set-mcdr! e1 e2)
e1

(mmemq e3 e1)

'(new investig)
e4
(eq? e4 e4)
(define unique (mcons e4 e5))
unique
(mmemq e4 unique)
(mmemq e5 unique)

;unique
;(mmemq e4 unique)
;(set-mcdr! unique e5)
;unique
;(mmemq e4 unique)
;(mmemq e5 unique)




(define t1 (mcons 'a 'b))
(define t2 (mcons 'c 'd))
(define t3 (mcons (mcons t1 t2) t2))

;(define (msearch mpair list-of-mpairs)
;  (if (eq? mpair list-of-mpairs)
;      (eq? mpair list-of-mpairs)
;      (if (eq? mpair (mcar list-of-mpairs))
;          (eq? mpair (mcar list-of-mpairs))
;          (msearch mpair (mcdr list-of-mpairs)))))

;(msearch t1 t3)
;(msearch t2 t2)
;(msearch t2 t3)

(mpair? t1)
(mpair? t3)
(mmemq L73 L71)

(define (count-mpairs-JS x)
  (let ((temp (mlist '())))
    (define (count-proc x)
      (if (not (mpair? x))
          0      
          (if (mmemq x temp)
            0
            (begin
              (set! temp (mcons x temp)) ;this was the line i could not do myself
              (+ (count-proc (mcar x))
                 (count-proc (mcdr x))
                 1)))))
    (count-proc x)))

(count-mpairs-JS L71)
(count-mpairs-JS linf)



;below the solutions from wizardbook
(define count-mpairs2
  (let ((seen null))
    (lambda (x)
      (cond ((not (pair? x)) 0)
            ((memq x seen) 0)
            (else (set! seen (cons x seen))
                  (+ (count-pairs (car x))
                     (count-pairs (cdr x))
                     1))))))

t1
t2
(set! t1 t2)
t1
t2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.18
'(Exercise 3.18)

(set-mcdr! t1 t1)
t1
(define t4 (mcons 2 3))
(set-mcar! t4 t4)
t4

(define (check-inf-loop x)
  (let ((seen x))
      (if (not (mpair? x))
          #f
          (if (eq? (mcdr x) seen)
              #t
              (check-inf-loop (mcdr x))))))
            
t1
(check-inf-loop t1)
(check-inf-loop t4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.19 - skipped (not feeling clever today)
'(Exercise 3.19)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Mutation is just assignment)
'(Mutation is just assignment)

(define (ccons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined 
                 operation: CONS" m))))
  dispatch)

(define (ccar z) (z 'car))
(define (ccdr z) (z 'cdr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.20
'(Exercise 3.20)

(define x3 (ccons 1 2))
(define z3 (ccons x3 x3))

(((ccdr z3) 'set-car!) 17)

(x3 'car)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Representing Queues
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.21
'(Exercise 3.21)

(define (print-queue queue)
  (mlist->list (front-ptr queue)))


(define q1 (make-queue))
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.22
'(Exercise 3.22)

(define (make-queue2)
  (let ((front-ptr '())
        (rear-ptr  '()))

    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?) 
      (null? front-ptr))
    (define (front-queue queue)
      (if ((empty-queue?))
          (error "FRONT called with an 
              empty queue" (mcons front-ptr rear-ptr))
          (mcar front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (mcons front-ptr rear-ptr))
              (else (set-mcdr! rear-ptr 
                               new-pair)
                    (set-rear-ptr! new-pair)
                    (mcons front-ptr rear-ptr)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with 
                 an empty queue" (mcons front-ptr rear-ptr)))
            (else (set-front-ptr! 
                   (mcdr front-ptr))
                  (mcons front-ptr rear-ptr))))
    (define (print-queue)
      (mlist->list front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr! )
            ((eq? m 'empty-queue?) empty-queue?  )
            ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)  
            (else
             (error "Undefined m" m))))
    dispatch))

'(data)
(define q0 (make-queue))
(insert-queue! q0 'a)
(print-queue q0)
(insert-queue! q0 'b)
(print-queue q0)
(delete-queue! q0)
(print-queue q0)
(delete-queue! q0)
(print-queue q0)

'(proc)
(define q2 (make-queue2))
((q2 'insert-queue!) 'a)
((q2 'print-queue))
((q2 'insert-queue!) 'b)
((q2 'print-queue))
((q2 'delete-queue!))
((q2 'print-queue))
((q2 'delete-queue!))
((q2 'print-queue))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.23
'(Exercise 3.23)

; constructor
(define (make-deque) (mcons '() '()))

;predicate (same as for queue)
(define (empty-deque? deque) 
  (null? (front-ptr deque)))

; selectors (alrady in queue)
;(define (front-ptr deque) (mcar deque))
;(define (rear-ptr deque) (mcdr deque))

(define (front-deque deque) ; same as for queue (only changes queue to deque)
  (if (empty-deque? deque)
      (error "FRONT called with an 
              empty deque" deque)
      (mcar (front-ptr deque))))

(define (rear-deque deque) ; this is new for deque
  (if (empty-deque? deque)
      (error "REAR called with an 
              empty deque" deque)
      (mcdr deque)))


; mutators (same as for queue)
;(define (set-front-ptr! deque item) 
;  (set-mcar! deque item))

;(define (set-rear-ptr! queue item) 
;  (set-mcdr! queue item))

(define (rear-insert-deque! deque item) ;sane as insert-queue! for queues, just changed names
  (let ((new-pair (mcons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else (set-mcdr! (rear-ptr deque) 
                          new-pair)
                (set-rear-ptr! deque new-pair)
                deque))))

(define (front-insert-deque! deque item) ;new
  (let ((new-pair (mcons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else (define temp deque)
                (set-front-ptr! deque (mcons item (front-ptr temp)));added
                ;(set! deque new-pair);added
                ;(set-front-ptr! deque new-pair)
                deque))))

(define (front-delete-deque! deque) ;sane as delete-queue! for queues, just changed names
  (cond ((empty-deque? deque)
         (error "DELETE! called with 
                 an empty deque" deque))
        (else (set-front-ptr! 
               deque 
               (mcdr (front-ptr deque)))
              deque)))

(define (reverse x) ;same as mystery function from exercise 3.13 (it mutates x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

(define (rear-delete-deque! deque) ;new - works, but it is not in O(1) but O(n) as we are reversing all items
  (cond ((empty-deque? deque)
         (error "DELETE! called with 
                 an empty deque" deque))
        (else (define inverted (reverse (front-ptr deque)))
              (set-rear-ptr! deque (mcar (mcdr inverted)))
              (set-front-ptr! deque (reverse (mcdr inverted)))
               deque)))


(define (print-deque deque) ; same as queue just changed name
  (mlist->list (front-ptr deque)))

;(define dummy (mlist 1 2 3 4 5))
;(define dummy_r (reverse dummy))
;dummy_r

(define deq (make-deque))
(print-deque deq)
(empty-deque? deq)
(rear-insert-deque! deq 'a)
(print-deque deq)
(rear-insert-deque! deq 'b)
(print-deque deq)
(rear-insert-deque! deq 'c)
(print-deque deq)
(front-insert-deque! deq 'A)
(print-deque deq)
(front-insert-deque! deq 'B)
(print-deque deq)
(rear-insert-deque! deq 'd)
(print-deque deq)
(front-delete-deque! deq)
(print-deque deq)
(rear-delete-deque! deq)
(print-deque deq)

; but come up with rear-delete-dequeue! that is O(1):
; my idea is that instead of last item of deque pointing to last item of the front-deque, it will point to one one last item of deque
; as you can get to the last from there relatively easily but not other way round (but that is utopia...)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3.3.3 Representing Tables
'(3.3.3 Representing Tables)

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (mcar (mcar records))) 
         (mcar records))
        (else (assoc key (mcdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                  (mcons (mcons key value) 
                        (mcdr table)))))
  'ok)

(define (make-table)
  (mlist '*table*))

(define my-table (make-table))
(insert! 'jarek 33 my-table)
(insert! 'marta 30 my-table)
my-table
(insert! 'jarek 34 my-table)
my-table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Two-dimensional tables
'(Two-dimensional tables)

(define (lookup1D key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        #f)))

(define (lookup2D key-1 key-2 table)
  (let ((subtable (assoc key-1 (mcdr table))))
    (if subtable
        (let ((record 
               (assoc key-2 (mcdr subtable))))
          (if record (mcdr record) #f))
        #f)))

(define (insert2D! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (mcdr table))))
    (if subtable
        (let ((record 
               (assoc key-2 (mcdr subtable))))
          (if record
              (set-mcdr! record value)
              (set-mcdr! 
               subtable
               (mcons (mcons key-2 value)
                     (mcdr subtable)))))
        (set-mcdr! 
         table
         (mcons (mlist key-1 (mcons key-2 value))
               (mcdr table)))))
  'ok)

(define age-height (make-table))
(insert2D! 'age 'jarek 33 age-height)
(insert2D! 'age 'marta 30 age-height)
(insert2D! 'height 'jarek 193 age-height)
(insert2D! 'height 'maly-zbych 210 age-height)
age-height
(insert2D! 'height 'jarek 192 age-height)
age-height


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Creating local tables
'(Creating local tables - procedural approach and using local state)

(define (make-local-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (mcdr subtable))))
              (if record (mcdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! 
                   subtable
                   (mcons (mcons key-2 value)
                         (mcdr subtable)))))
            (set-mcdr! 
             local-table
             (mcons (mlist key-1
                         (mcons key-2 value))
                   (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

;now implement the get and put operations used in 2.4.3 for data-directed programming
(define operation-table (make-local-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;examples of using local-tables
(define ah (make-local-table))
((ah 'insert-proc!) 'age 'jarek 33)
((ah 'insert-proc!) 'age 'marta 30)
((ah 'insert-proc!) 'height 'jarek 195)
((ah 'insert-proc!) 'height 'maly-zbych 210)
ah ;this will just say it is a procedure, we need a new dispatch fn for showing 
((ah 'insert-proc!) 'height 'jarek 192)
ah

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.24
'(Exercise 3.24)

(define (assoc-num key records)
  (cond ((null? records) #f)
        ((<= (abs (- key (mcar (mcar records)))) 0.1)
         (mcar records))
        (else (assoc-num key (mcdr records)))))


(define (make-num-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc-num key-1 (mcdr local-table))))
        (if subtable
            (let ((record 
                   (assoc-num key-2 
                          (mcdr subtable))))
              (if record (mcdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc-num key-1 (mcdr local-table))))
        (if subtable
            (let ((record 
                   (assoc-num key-2 
                          (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! 
                   subtable
                   (mcons (mcons key-2 value)
                         (mcdr subtable)))))
            (set-mcdr! 
             local-table
             (mcons (mlist key-1
                         (mcons key-2 value))
                   (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define numeric-key (make-num-table))
((numeric-key 'insert-proc!) 10 1 33)
((numeric-key 'insert-proc!) 10 2 30)
((numeric-key 'insert-proc!) 12 1 195)
((numeric-key 'insert-proc!) 12 3 210)
((numeric-key 'lookup-proc) 12 3)
((numeric-key 'insert-proc!) 12.01 3 212)
((numeric-key 'lookup-proc) 12 3)
((numeric-key 'lookup-proc) 12.1 3) ;within tolerance of o.1
((numeric-key 'lookup-proc) 12.11 3) ;outside tolerance of o.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.25 - arbitrary number of keys (not just one or two dim as above)
'(Exercise 3.25)


