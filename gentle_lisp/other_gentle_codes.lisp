;;Exercise 8.35
(defun my-set-diff (first second)
	(cond ((null first) nil)
		  ((member (car first) second) (my-set-diff (cdr first) second))
		  (t (cons (car first) (my-set-diff (cdr first) second)))))


		  
;;Exercise 8.36 - conditional augmentation
(defun count-odd (lst)
	(cond ((null lst) 0)
		  ((oddp (car lst)) (+ 1 (count-odd (cdr lst))))
		  (t (count-odd (cdr lst)))))
		  
(defun count-odd-r (lst) ;regular
	(cond ((null lst) 0)
		  (t (+ (if (oddp (car lst))
					1
					0) 
			    (count-odd-r (cdr lst))))))
				
;;Exercise 8.39
(defun cnt-atoms (x)
	(cond ((null x) 0)
		  ((atom x) 1)
		  (t (+ (cnt-atoms (car x))
				(cnt-atoms (cdr x))))))
				
;;Exercise 8.41
(defun sum-tree (x)
	(cond ((null x) 0)
		  ((numberp x) x)
		  ((atom x) 0)
		  (t (+ (sum-tree (car x))
				(sum-tree (cdr x))))))

;;Exercise 8.42
(defun my-subst (new old str)
	(cond ((null str) nil)
		  ((equal old (car str)) (cons new (my-subst new old (cdr str))))
		  (t (cons (car str) (my-subst new old (cdr str))))))
		  
;;Exercise 8.43
(defun flatten (x)
	(cond ((null x) nil)
		  ((atom x) (cons x nil))
		  (t (append (flatten (car x))
				     (flatten (cdr x))))))

;;Exercise 8.44
(defun tree-depth (x)
	(cond ((null x) 0)
		  ((atom x) 0)
		  (t (max (+ 1 (tree-depth (car x)))
				  (+ 1 (tree-depth (cdr x)))))))
				  
;;Exercise 8.45
(defun paren-depth (x)
	(cond ((null x) 0)
		  ((atom x) 0)
		  (t (max (+ 1 (paren-depth (car x)))
				  (paren-depth (cdr x))))))

;;Exercise 8.46
(defun count-up (n)
	(count-up-recursively 1 n))
	
(defun count-up-recursively (cnt n)
	(cond ((> cnt n) nil)
		  (t (cons cnt
			       (count-up-recursively (+ cnt 1) n)))))
				   
(defun count-up-2 (n)
	(cond ((zerop n) nil)
		  (t (append (count-up-2 (- n 1))
					 (list n)))))
				
;;Exercise 8.47				
(defun make-loaf (n)
	(if (zerop n)
		nil
		(cons 'X (make-loaf (- n 1))))) 
		
;;Exercise 8.48				
(defun bury (sym n)
	(if (zerop n)
		sym
		(cons (bury sym (- n 1)) nil)))
		
;;Exercise 8.49
(defun pairings (a b)
	(if (and (null a) (null b))
		nil
		(cons (list (car a) (car b)) (pairings (cdr a) (cdr b)))))
		
;;Exercise 8.50
(defun sublists (str)
	(if (null str) 
		nil
		(cons (member (car str) str) (sublists (cdr str)))))
		
;;Exercise 8.51
(defun my-reverse (str)
	(rev-helper str nil))
	
(defun rev-helper (start end)
	(cond ((null start) end)
		  (t (rev-helper (cdr start) (cons (car start) end)))))
	
;;Exercise 8.52
(defun my-union (first second)
	(append first 
			(my-set-diff second first))) 
			
;;Exercise 8.53
(defun largest-even (str)
	(cond ((null str) 0)
		  ((evenp (car str)) (max (car str) (largest-even (cdr str))))
		  (t (largest-even (cdr str)))))

;;Exercise 8.54
(defun huge (x)
	(h-helper 1 x))

(defun h-helper (start n)
	(if (> start n)
		1
		(* n (h-helper (+ start 1) n)))) 

;;Exercise 8.56
(defun every-other (str)
	(cond ((null str) nil)
		  (t (cons (car str) (every-other (cddr str))))))

;;Exercise 8.57		  
(defun left-half (str)
	(lh-helper 1 (ceiling (/ (length str) 2.0)) str))
	
(defun lh-helper (start end items)
	(if (> start end)
		nil
		(cons (car items) (lh-helper (+ start 1) end (cdr items)))))
	
;;Exercise 8.58
(defun merge-lists (f s)
	(cond ((and (null f) (null s)) nil)
		  ((null f) (cons (car s) (merge-lists f (cdr s))))
		  ((null s) (cons (car f) (merge-lists (cdr f) s)))
		  ((< (car f) (car s)) (cons (car f) (merge-lists (cdr f) s)))
		  (t (cons (car s) (merge-lists f (cdr s))))))

;;Exercise 8.64
(defun tree-find-if (fn x)
	(cond ((null x) nil)
		  ((and (atom x) (funcall fn x)) x)
		  ((atom x) nil)
		  (t (or (tree-find-if fn (car x)) (tree-find-if fn (cdr x))))))
		  
;;Exercise 8.65a

(defun tr-reverse (x)
;tail-recursive (p.280)
	(tr-rev1 x nil))

(defun tr-rev1 (x result)
;tail-recursive (p.280)
	(cond ((null x) result)
		  (t (tr-rev1 (rest x)
					  (cons (first x) result)))))
					  
(defun tr-reverse-2 (x)
;written with labels
	(labels ((tr-rev2 (x result)
					   (cond  ((null x) result)
							  (t (tr-rev2 (rest x)
										  (cons (first x) result))))))
		(tr-rev2 x nil)))
	

;;Exercise 8.65b
(defun tr-count-slices (loaf)
;tail-recursive (p.280)
	(tr-cs1 loaf 0))

(defun tr-cs1 (loaf n)	
;tail-recursive (p.280)
	(cond ((null loaf) n)
		  (t (tr-cs1 (rest loaf) (+ n 1)))))	

(defun tr-count-slices-2 (loaf)
;written with labels
	(labels ((tr-cs2 (loaf n)
		(cond ((null loaf) n)
			  (t (tr-cs2 (rest loaf) (+ n 1))))))
	  (tr-cs2 loaf 0)))
	  
;;Exercise 8.66
(defun arith-eval (x)
	(eval (list 
		(cadr x)
		(if (atom (car x))
			(car x)
			(arith-eval (car x)))
		(if (atom (caddr x))
			(caddr x)
			(arith-eval (caddr x))))))

;;Exercise 8.67

(defun term (x)
	(if (atom x)
		(numberp x)
		(legalp x)))

(defun legalp (x)
	(if (atom x)
		(numberp x)
		(and (symbolp (cadr x)) (term (car x)) (term (caddr x)))))
		
;;Exercise 8.69
(defun prime-fact (n)
	(labels ((prime-help (start stop left)
				(cond ((> start stop) nil)
					  ((zerop (mod left start))
							(cons start (prime-help start (floor (/ stop 2)) (/ left start))))
					  (t (prime-help (+ start 1) stop left)))))
		(prime-help 2 n n)))
		

;;Exercise 8.70
(defun factors (n)
	(factors-help n 2))

(defun factors-help (n p)
	(cond ((equal n 1) nil)
		  ((zerop (rem n p))
			      (cons p (factors-help (/ n p) p)))
				  (t (factors-help n (+ p 1)))))
	
;to do factor-tree
(defun factor-tree (n)
	(ft-help n 2))

(defun ft-help (n p)
	(cond ((equal n 1) nil)
		  ((zerop (rem n p))
			      (if (equal 1 (/ n p))
					p
					(list n p (ft-help (/ n p) p))))
		  (t (ft-help n (+ p 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; CHAPTER 9    ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Exercise 9.2
(defun draw-line (n)
	(labels ((helper (start)
				(format t "*")
				(if (< start n)
					(helper (+ start 1))
					(format t "~%"))))				
		(helper 1)))
					
	
;;Exercise 9.3
(defun draw-box (c r)
	(draw-line c)
	(format t "~&")
	(if (> r 1)
		(draw-box c (- r 1))))
		
;;Exercise 9.4
(defun ninty-nine-bottles (n)
	(format t "~&~A bottles of beer on the wall," n)
	(format t "~&~A bottles of beer!" n)
	(format t "~&Take one down,")
	(format t "~&Pass it around,")
	(if (> n 1)
		(ninty-nine-bottles (- n 1))
		(format t "~&Now onto bottles of rum!")))
		
;;Exercise 9.5
(defun mynth (n x)
	(setq empty " ")
	(if (null (nth n x))
		empty
		(nth n x)))

(defun print-board (bd)
	(format t "~& ~A | ~A | ~A" (mynth 0 bd) (mynth 1 bd) (mynth 2 bd))
	(format t "~& ---------")
	(format t "~& ~A | ~A | ~A" (mynth 3 bd) (mynth 4 bd) (mynth 5 bd))
	(format t "~& ---------")
	(format t "~& ~A | ~A | ~A" (mynth 6 bd) (mynth 7 bd) (mynth 8 bd)))
	

;;Exercise 9.6
(defun wage ()
	(format t "Let me know your hourly rate: ")
	(let ((rate (read)))
		(format t "Thanks, now let me know your hours per week you work: ")
			(let ((week (read)))
				(format t "Annual wage is ~A ~%" (* 52 week rate)))))

;;Exercise 9.7
(defun cookie-monster ()
	(format t "~&Give me cookie!!! ~%Cookie? ")
	(let ((item (read)))
		(if (equal item 'cookie)
			(format t "Thank you!... Munch munch munch...BURP")
			(progn (format t "No want no ~A...~%~%" item) (cookie-monster))))) 
		
;;Example y-or-n-p or yes-or-no-p from section 9.5
(defun riddle ()
	(if (yes-or-no-p "Do you seek Zen enlightenment? ")
		(format t "Then do not ask for it!")
		(format t "You have found it.")))

	
;;Example from section 9.6/9.7 on reading/writing files with with-open-file
(defun get-tree-data ()
	(with-open-file (stream "timber.dat")
	(let* ((tree-loc (read stream))
		   (tree-table (read stream))
		   (num-trees (read stream)))
				(format t "~&There are ~S trees on ~S." num-trees tree-loc)
				(format t "~&They are: ~S" tree-table))))

	
(defun save-tree-data (tree-loc tree-table num-trees)
	(with-open-file (stream "timber2.dat" :direction :output)
		(format stream "~S~%" tree-loc)
		(format stream "~S~%" tree-table)
		(format stream "~S~%" num-trees)))
		

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; CHAPTER 10   ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;;Exercise 10.2
(defvar *total-glasses* 0)
(defun sell (n)
	"Ye Olde Lemonade Stand: Sales by the Glass."
	(setf *total-glasses* (+ *total-glasses* n))
	(format t "~&That makes ~S glasses so far today." *total-glasses*))
	

(defvar *total-glasses-2* 0)
(defun sell-2 (n)
	"Ye Olde Lemonade Stand: Sales by the Glass."
	(incf *total-glasses-2* n)
	(format t "~&That makes ~S glasses so far today." *total-glasses-2*))
	
	
;;Exercise 10.3
(setf *friends* nil)
(setf *fr-cnt* 0)

(defun meet (person)
	(cond ((equal person (first *friends*))
				'we-just-met
				(incf *fr-cnt*))
		  ((member person *friends*)
				'we-know-each-other
				(incf *fr-cnt*))
		  (t (push person *friends*)
				'pleased-to-meet-you)))
	
;;Exercise 10.4
(defun forget (person)
	(if (member person *friends*)
		(remove-if #'(lambda (x) (equal person x)) *friends*)
		'You-have-not-met-this-person-yet))
	
;;Example
(defun get-name ()
	(let ((last-name nil)
		  (first-name nil)
		  (middle-name nil)
		  (title nil))
		(format t "~&Last name? ")
		(setf last-name (read))
		(format t "~&First name? ")
		(setf first-name (read))
		(format t "~&Middle name or initial? ")
		(setf middle-name (read))
		(format t "~&Preferred title? ")
		(setf title (read))
		(list title first-name middle-name last-name)))
	
(defun analyze-profit (price commission-rate)
	(let* ((commission (* price commission-rate))
		   (result
				(cond ((> commission 100) 'rich)
					  ((< commission 100) 'poor))))
		(break "Value of RESULT is ~S" result) ; this will prompt a debugger
		(format t "~&I predict you will be: ~S" result) 
		result))

(defun average (x y)
	(unless (and (numberp x) (numberp y))
		(error "Arguments must be numbers: ~S, ~S" x y))
	(/ (+ x y) 2.0))


;;Exercise 10.10

(defun ntack (lst symb)
	(nconc lst (list symb)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; CHAPTER 11   ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Exercise 11.1
(defun it-member (item x)
	(dolist (i x nil)
		(if (equal i item) (return 't))))

;;Exercise 11.2		
(defun it-assoc (find items)
	(dolist (i items nil)
		(if (equal (car i) find) (return i))))

;;Exercise 11.3
(defun check-all-odd (lista)
	(cond ((null lista) 't)
		  ((oddp (car lista)) 
		    (format t "~&Checking ~S..." (car lista))
			(check-all-odd (cdr lista)))
		  (t 
		    (format t "~&Checking ~S..." (car lista))
			nil)))
			
;;Exercise 11.4
(defun it-length (x)
	(let ((len 0))
		(dolist (item x len)
			(setf len (+ len 1)))))
			
;;Exercise 11.5
(defun it-nth (index x)
	(let ((iteration 0))
		(dolist (element x)
			(if (equal iteration index) 
				(return element) 
				(setf iteration (+ 1 iteration))))))
			
;;Exercise 11.6
(defun it-union (s1 s2)
	(let ((output s1))
		(dolist (elt s2 output)
			(when (not (member elt s1)) 
				(push elt output)))))

;;Exercise 11.7
(defun it-intersection (x y)
	(let ((result-set nil))
		(dolist (element x (reverse result-set))
			(when (member element y)
				(push element result-set)))))

;;Exercise 11.8
(defun it-reverse (x)
	(let ((out nil))
		(dolist (element x out)
			(push element out))))
			
;;Exercise 11.9
(defun check-all-odd-orig (list-of-numbers)
	(dolist (e list-of-numbers t)
		(format t "~&Checking ~S..." e)
		(if (not (oddp e)) (return nil))))

(defun check-all-odd-old2 (lista)
	(cond ((null lista) 't)
		  ((oddp (car lista)) 
		    (format t "~&Checking ~S..." (car lista))
			(check-all-odd-old2 (cdr lista)))
		  (t 
		    (format t "~&Checking ~S..." (car lista))
			nil)))

(defun check-all-odd-new (lista)
	(do ((end (- (length lista) 1))
		 (iter 0 (+ iter 1)))
		((> iter end) 't)
		(format t "~&Checking ~S..." (nth iter lista))
		(when (not (oddp (nth iter lista)))
			(return nil))))
			
;;Exercise 11.10
(defun launch (n)
	(do ((cnt n (- cnt 1)))
		((zerop cnt) (format t "Blast off!"))
		(format t "~S..." cnt)))
		
(defun launch-new (n)
	(let ((end (- n 1)))
		(dotimes (i n)
			(if (equal i end)
				(format t "~S...Blast off!" (- n i))
				(format t "~S..." (- n i))))))
				

;;Exercise 11.11
(defun find-largest (list-of-numbers)
	(let ((largest (first list-of-numbers)))
		(dolist (element (rest list-of-numbers) largest)
			(when (> element largest)
				(setf largest element)))))

(defun find-largest-star (list-of-numbers)
	(do* ((elts list-of-numbers (cdr elts)) 
		  (largest (car elts)))
		 ((null elts) largest)
		 (when (> (car elts) largest)
				(setf largest (car elts)))))				
		
;;Exercise 11.12
(defun power-of-2 (n) ;2 to the Nth power.
	(let ((result 1))
		(dotimes (i n result)
			(incf result result))))
		
(defun power-of-2-do (n) ;2 to the Nth power.
	(do ((result 1 (+ result result))
		  (i 0 (+ i 1)))
		((equal i n) 
			(return result))))
			
;;Exercise 11.13
(defun first-non-integer (x)
"Return the first non-integer element of X."
	(do* ((z x (rest z))
		  (z1 (first z) (first z)))
		 ((null z) 'none)
		 (unless (integerp z1)
			(return z1))))
			
(defun first-non-integer-dolist (x)
	(dolist (elt x 'none)
		(unless (integerp elt)
			(return elt))))

;;Exercise 11.15 (spot a bug)
(defun ffo-with-do (x)
	(do ((z x (rest z))
		 (e (first x) (first z)))
		((null z) nil)
		(if (oddp e) (return e))))

;;Example 11.10 - read with infinite do loop
(defun read-a-number ()
	(do ((answer nil))
		(nil)
		(format t "~&Please type a number: ")
		(setf answer (read))
		(if (numberp answer)
			(return answer))
		(format t "~&Sorry, ~S is not a number. Try again." answer)))

;;Exercise 11.18
(defun run1 ()
	(dotimes (i 5 i)
		(format t "~&I = ~S" i)))

(defun run2 ()
	(do ((i 0 (+ i 1)))
		((equal i 5) i)
		(format t "~&I = ~S" i)))



;;Exercise 11.21
(defun i-fib (n)
	(do ((count 0 (+ count 1))
		(fst 1)
		(snd 1)
		(temp 0))
		((equal count n) fst)
		(setf temp (+ fst snd))
		(setf fst snd)
		(setf snd temp)))
		 
; answers to above
(defun fib1 (n) ; version with DO*
	(do* ((cnt 0 (+ cnt 1))
		 (i 1 j)
		 (j 1 k)
		 (k 2 (+ i j)))
		((equal cnt n) i)))

(defun fib2 (n) ; version with DO
	(do ((cnt 0 (+ cnt 1))
		 (i 1 j)
		 (j 1 (+ i j)))
		((equal cnt n) i)))

		

(defun addup (n)
"Adds up the first N integers"
(do 
	((i 0 (+ i 1))
	 (sum 0 (+ sum i)))
((> i n) sum)))


(defun make-sundae (name &key (size 'regular)
							  (ice-cream 'vanilla)
							  (syrup 'hot-fudge)
							  nuts
							  cherries
							  whipped-cream)
	(list 'sundae
		(list 'for name)
		(list ice-cream 'with syrup 'syrup)
		(list 'toppings '=
			(remove nil
				(list (and nuts 'nuts)
					  (and cherries 'cherries)
					  (and whipped-cream
					  'whipped-cream))))))
					  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; CHAPTER 12   ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct starship 
	(name nil)
	(speed 0)
	(condition 'green)
	(shields 'down))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; CHAPTER 13   ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *print-array* t) ; in order to see the inside of vector printed / otherwise use nil instead of t

(setf my-vec '#(tuning violin 440 a))

(aref my-vec 1)

;;Exercise 13.1

(setf (get 'alpha 'fooprop) '(a b c d e))

(defun subprop (sym elem prop)
	(setf (get sym prop) (remove elem (get sym prop)))) 
	
;;Exercise 13.2
(defun addprop (sym elem prop)
	(pushnew elem (get sym prop)))

(defun record-meeting (x y)
	(addprop x y 'has-met)
	(addprop y x 'has-met))

(defun forget-meeting (x y)
	(subprop x y 'has-met)
	(subprop y x 'has-met))

(record-meeting 'clare 'bill)
(record-meeting 'clare 'maria)
(record-meeting 'clare 'jo)
(record-meeting 'jo 'bill)

;;Exercise 13.3
(defun my-get (sym prop)
	(cadr (member prop (symbol-plist sym))))

;;Exercise 13.4
(defun hasprop (sym prop)
	(if (eq (get sym prop 'unknown) 'unknown)
		nil
		t))

(setf (get 'clare 'age) 22)
(setf (get 'clare 'height) 170)
(setf (get 'clare 'car) 'yes)		
(setf (get 'clare 'siblings) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; CHAPTER 14   ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro simple-incf (var)
	(list 'setq var (list '+ var 1)))

		
;;Exercise 14.4
(defmacro simple-rotatef (a b)
	`(let ((t1 ,a) (t2 ,b))
		(setq ,a t2)
		(setq ,b t1)))

;Exercise 14.5
(defmacro set-mutual (a b)
	`(let ((t1 ',a) (t2 ',b))
		(setq ,a t2)
		(setq ,b t1)))
		
;;Exercise 14.6
(defmacro set-zero (&rest variables)
	`(progn ,@(mapcar #'(lambda (var) (list 'setf var 0)) variables)
		'(zeroed ,@variables)))

(defmacro variable-chain (&rest variables)
	`(progn ,@(mapcar #'(lambda (var1 var2) `(setq ,var1 ',var2)) variables (cdr variables))
		'(chained ,@variables)))
		
(defun tedious-sqrt (n)
	(dotimes (i n)
		(if (> (* i i) n) (return i))))

(defmacro bad-announce-macro ()
	(format t "~%Hi mom!"))
	
(defmacro good-announce-macro ()
	`(format t "~%Hi mom!"))

(defun say-hi ()
	(good-announce-macro))
	
(defmacro while (test &body body)
	`(do ()
		 ((not ,test))
		,@body))

(defun next-power-of-two (n &aux (i 1))
	(while (< i n)
		(format t "~%Not ~A" i)
		(setf i (* i 2)))
	i)