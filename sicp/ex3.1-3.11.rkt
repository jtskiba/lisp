#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This is in relation to chapter 3.1.1
'(This is in relation to chapter 3.1.1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;lets make withdraw that has local state which is balance
'(lets make withdraw that has local state which is balance)


(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 25)
(withdraw 25)
(withdraw 60)
(withdraw 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;lets make balance local to withdraw
'(lets make balance local to withdraw)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "insufficient funds"))))

(new-withdraw 25)
(new-withdraw 25)
(new-withdraw 60)
(new-withdraw 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lets make withdrawal processor where you can specify a balance
'(lets make withdrawal processor where you can specify a balance)

(define (make-withdraw balance)
  (lambda (amount)
     (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50)
(W2 70)
(W2 40)
(W1 40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now lets create objects which can handle both witdrawals and deposits
;; this is an example of message passing programming
'(now lets create objects which can handle both witdrawals and deposits)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request:
                        MAKE ACCOUNT:" m))))
  dispatch)

(define W3 (make-account 100))
((W3 'withdraw) 25)
((W3 'deposit) 10)
((W3 'withdraw) 90)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.1
'(Exercise 3.1)

(define (make-accumulator balance)
  (lambda (amount)
    (set! balance (+ balance amount))
    balance))

(define A (make-accumulator 5))
(A 10)
(A 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.2
'(Exercise 3.2)

(define (make-monitored f)
  (let ((counter 0))
    (define (mf input)
      (set! counter (+ counter 1))
      (f input))
    (lambda (value)
      (cond ((eq? value 'how-many-calls?) counter)
            ((eq? value 'reset-count) (set! counter 0))
            (else (mf value))))))

(sqrt 100)
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 4)
(s 16)
(s 'how-many-calls?)
(s 'reset-count)
(s 25)
(s 36)
(s 'how-many-calls?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.3
'(Exercise 3.3)

(define (make-account2 balance my-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect amount)
    "Incorrect password")
  (define (dispatch entered-password m)
    (cond ((eq? entered-password my-password)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknown request:
                        MAKE ACCOUNT:" m))))
          (else incorrect)))
    dispatch)

(define acc (make-account2 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 20)
((acc 'secret-password 'deposit) 20)
((acc 'secret-password 'withdraw) 200)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.4
'(Exercise 3.4)

(define (make-account3 balance my-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (make-monitored f)
  (let ((counter 0))
    (define (call-the-cops input)
      "We are calling the cops!!")
    (define (mf input)
      (set! counter (+ counter 1))
      (if (>= counter 7)
          (call-the-cops input)
          (incorrect input)))
    (lambda (value)
      (cond ((eq? value 'my-password) (set! counter 0))
            (else (mf value))))))
  
  (define (incorrect amount)
    "Incorrect password")
  (define monitored-incorrect (make-monitored incorrect))
  (define (dispatch entered-password m)
    (cond ((eq? entered-password my-password)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknown request:
                        MAKE ACCOUNT:" m))))
          (else monitored-incorrect)))
    dispatch)

(define acc2 (make-account3 100 'secret-password))
((acc2 'secret-password 'withdraw) 40)
((acc2 'secret-password 'deposit) 20)
((acc2 'secret-password 'withdraw) 50)
((acc2 'some-other-password 'deposit) 20)
((acc2 'some-other-password 'deposit) 20)
((acc2 'some-other-password 'deposit) 20)
((acc2 'some-other-password 'deposit) 20)
((acc2 'some-other-password 'deposit) 20)
((acc2 'some-other-password 'deposit) 20)
((acc2 'some-other-password 'deposit) 20)

;; took me 3h to get here (most spent reading and last exercise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This is in relation to chapter 3.1.2
'(This is in relation to chapter 3.1.2)

(define (rand-update x)
  (let ((a 17) (b 24) (m 100001))
    (modulo (+ (* a x) (* b x)) m)))

;(rand-update 13)

(define rand-init 45)

(define rand
  (let ((x rand-init))
    (lambda () (set! x (rand-update x)) x)))

(rand)
(rand)
(rand)
(rand)
(rand)

; this is Montecarlo simulation if we are to rely on the rand which has got local state built in
'(this is Montecarlo simulation if we are to rely on the rand which has got local state built in)

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials 
                          cesaro-test))))

(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(estimate-pi 10000)

; this is Montecarlo simulation if we are to rely on directly on rand-update ie no local state saved between runs
'(this is Montecarlo simulation if we are to rely on directly on rand-update ie no local state saved between runs)

(define random-init 45)

(define (estimate-pi2 trials)
  (sqrt (/ 6 (random-gcd-test trials 
                              random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining 
                trials-passed 
                x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))

(estimate-pi2 10000)
; the reason the two Pi estimates are different is because if we have a sequence of pseudo random x1, x2, x3, x4, x5 ...
; then the first option calculates pass/fail on gcd's of x1 vs x2, x3 vs x4, x5 vs x6 and so on
; whereas the second option calcs pass/fail on gcd's of x1 vs x2, x2 vs x3, x3 vs x4, and so on.. hence different results


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.5
'(Exercise 3.5)

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (circle x y)
  (<= (+ (sqr (- x 5)) (sqr (- y 7))) (sqr 3)))

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (define (iter remain-trials trials-passed)
    (cond ((= remain-trials 0)
           (* (/ trials-passed trials)
              (exact->inexact (* (- y2 y1) (- x2 x1)))))
          ((predicate (random-in-range x1 x2)
                      (random-in-range y1 y2))
           (iter (- remain-trials 1) (+ trials-passed 1)))
          (else
           (iter (- remain-trials 1) trials-passed))))
  (iter trials 0))


(define x1 2)
(define x2 8)
(define y1 4)
(define y2 10)
(define area (estimate-integral circle x1 x2 y1 y2 2000))
area
(define pi (/ area (sqr 3)))
pi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.6
'(Exercise 3.6)

;(define rand-init 45) ;define above already

;(define rand
;  (let ((x rand-init))
;    (lambda () (set! x (rand-update x)) x)))

(define rand2
  (let ((x rand-init))
    (lambda (m)
      (if (eq? m 'generate)
          (set! x (rand-update x))
          (if (eq? m 'reset)
              (set! x (rand-update rand-init))
              ((error "Don't understand m:" m))))
      x)))

(rand2 'generate)
(rand2 'generate)
(rand2 'generate)
(rand2 'generate)
(rand2 'reset)
(rand2 'generate)
(rand2 'generate)
;(rand2 'gener)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This is in relation to chapter 3.1.3
'(This is in relation to chapter 3.1.3)

;factorial in functional (iterative) style
(define (factorial_it n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;factorial in imperative style (c like)
; problem here sis that you need to worry about order of set!s (different order => different results)
(define (factorial_im n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter 
                                  product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

(factorial_it 5)
(factorial_im 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.7
'(Exercise 3.7)

(define (make-acc balance my-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (tell-password)
    my-password)
  (define (tell-balance)
    balance)
  (define (incorrect [amount 1])
    "Incorrect password")
  (define (dispatch entered-password m)
    (cond ((eq? entered-password my-password)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'tell-password) tell-password)
                 ((eq? m 'tell-balance) tell-balance)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknown request:
                        MAKE ACCOUNT:" m))))
          (else incorrect)))
  dispatch)

(define (make-joint account password new-password)
  (define (tell-password)
    new-password)
  (define (dispatch entered-password m)
    (cond ((eq? entered-password new-password)
           (cond ((eq? m 'withdraw) (account ((account password 'tell-password)) 'withdraw))
                 ((eq? m 'tell-password) tell-password)
                 ((eq? m 'tell-balance) (account ((account password 'tell-password)) 'tell-balance))
                 ((eq? m 'deposit) (account ((account password 'tell-password)) 'deposit))
                 (else (error "Unknown request:
                        MAKE ACCOUNT:" m))))
          (else (account ((account password 'tell-password)) 'incorrect))))
  (if (eq? password
           ((account password 'tell-password)))
      dispatch
      "Incorrect password to make-joint account"))

(define peter-acc (make-acc 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 40)
((peter-acc 'other-password 'deposit) 20)
((peter-acc 'open-sesame 'deposit) 20)
((peter-acc 'open-sesame 'withdraw) 10)

((peter-acc 'open-sesame 'tell-password))
((peter-acc 'open-sesame 'tell-balance))

(define paul-acc (make-joint peter-acc 'open-sesame 'sesame-open))
((paul-acc 'sesame-open 'tell-balance))
((paul-acc 'sesame-open 'tell-password))
((paul-acc 'sesame-open 'deposit) 12)
((peter-acc 'open-sesame 'tell-balance))
((peter-acc 'sesame-open 'tell-password))
((peter-acc 'open-sesame 'tell-password))
((paul-acc 'sesame-open 'tell-password))
((peter-acc 'open-sesame 'deposit) 50)
((peter-acc 'open-sesame 'tell-balance))
((paul-acc 'sesame-open 'tell-balance))
((paul-acc 'sesame-open 'withdraw) 22)
((peter-acc 'open-sesame 'tell-balance))
((paul-acc 'sesame-open 'tell-balance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.8
'(Exercise 3.8)

;(define (make-monitored2 f)
;  (let ((counter 0))
;    (define (mf input)
;      (set! counter (+ counter 1))
;      (f input))
;    (lambda (value)
;      (cond ((eq? value 'how-many-calls?) counter)
;            ((eq? value 'reset-count) (set! counter 0))
;            (else (mf value))))))
;
;(define (funct number)
;  number)
;
;(define f (make-monitored funct))
;
;(define (f number)

(define count 0)

(define (f number)
  (set! count (+ count 1))
  (if (= count 1)
      number
      0))

;(f 0) ; 0           ; (f 1) = 1
;(f 1) ; 0 = 0       ; (f 0) = 0

;(f 1)
;(f 0)

(+ (f 0) (f 1))
; if we start from left to right:
; (f 0) ->> 0
; (f 1) ->> 0
; which should bring 0 in total

;if we start from right to left:
; (f 1) ->> 1
; (f 0) ->> 0
; which brings 1 in total

; given we received 0 from Racket, evaluation is done from left to right

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Exercise 3.9-11 - skipped here
'(Exercise 3.9-11 - skipped here)
; https://wizardbook.wordpress.com/2010/12/14/exercise-3-9/
; https://wizardbook.wordpress.com/2010/12/14/exercise-3-10/
; https://wizardbook.wordpress.com/2010/12/14/exercise-3-11/


