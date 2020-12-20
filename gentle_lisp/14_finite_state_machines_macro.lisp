; Source: COMMON LISP: A Gentle Introduction to Symbolic Computation by David S. Touretzky

; Chapter 14 - Macros and Compilation - Finite State Machines case study on vending machine

; "Finite state machines (FSMs) are a technique from theoretical computer
; science for describing how simple devices like vending machines or traffic
; lights work."

(defstruct (noda (:print-function print-noda))
	(name nil)
	(inputs nil)
	(outputs nil))

(defun print-noda (noda stream depth)
	(format stream "#<Noda ~A>" (noda-name noda)))

(defstruct (ark (:print-function print-ark))
	(from nil)
	(to nil)
	(label nil)
	(action nil))
	
(defun print-ark (ark stream depth)
	(format stream "#<ARK ~A / ~A / ~A>" 
		(noda-name (ark-from ark))
		(ark-label ark)
		(noda-name (ark-to ark))))

(defvar *nodas*)
(defvar *arks*)
(defvar *current-noda*)

(defun initialize ()
	(setf *nodas* nil)
	(setf *arks*  nil)
	(setf *current-noda* nil))

(defmacro defnoda (name)
"The DEFNODA macro is a bit of ‘‘syntactic sugar’’ for defining new nodas."
	`(add-noda ',name))
	
(defun add-noda (name)
"constructs a new noda with the given name and adds it to the list kept in the global variable *NODAS*."
	(let ((new-noda (make-noda :name name)))
		(setf *nodas* (nconc *nodas* (list new-noda)))
		new-noda))

(defun find-noda (name)
"Takes a noda name as input and returns the corresponding noda."
	(or (find name *nodas* :key #'noda-name)
		(error "No noda names ~A exists." name)))

(defmacro defark (from label to &optional action)
"macro provides a convenient syntax for defining arcs"
	`(add-ark ',from ',label ',to ',action))
	
(defun add-ark (from-name label to-name action)
"When an arc is created, it is added to the NODA-OUTPUTS list of the from node and the NODA-INPUTS
list of the to noda. It is also added to the list kept in the global variable *ARKS*."
	(let* ((from (find-noda from-name))
		   (to (find-noda to-name))
		   (new-ark (make-ark :from from
							  :label label
							  :to to
							  :action action)))
		(setf *arks* (nconc *arks* (list new-ark)))
		(setf (noda-outputs from)
			  (nconc (noda-outputs from)
					 (list new-ark)))
		(setf (noda-inputs to)
			  (nconc (noda-inputs to)
					 (list new-ark)))
		new-ark))

(defun fsm (&optional (starting-point 'start))
"top-level function FSM. It takes an optional input specifying the initial state of the machine"
	(setf *current-noda* (find-noda starting-point))
	(do ()
		((null (noda-outputs *current-noda*)))
	  (one-transition)))
	  
(defun one-transition ()
"FSM repeatedly calls the function ONE-TRANSITION to move to the next state."
	(format t "~&State ~A. Input: " (noda-name *current-noda*))
	(let* ((ans (read))
		   (ark (find ans
					  (noda-outputs *current-noda*)
					  :key #'ark-label)))
		(unless ark 
			(format t "~&No arc from ~A has label ~A.~%"
					   (noda-name *current-noda*) ans)
			(return-from one-transition nil))
		(let ((new (ark-to ark)))
			(format t "~&~A" (ark-action ark))
			(setf *current-noda* new))))
					   

;; Below done as part of ex14.7
(initialize)
(defnoda start)
(defnoda have-5)
(defnoda have-10)
(defnoda have-15)
(defnoda have-20)
(defnoda have-25)
(defnoda end)

(defark start nickel have-5 "Clunk!")
(defark start dime have-10 "Clink!")
(defark start quarter have-25 "Ker-chunk!")
(defark start coin-return start "Nothing to return.")
(defark have-5 nickel have-10 "Clunk!")
(defark have-5 dime have-15 "Clink!")
(defark have-5 quarter have-25 "Nickel change.")
(defark have-5 coin-return start "Returned five cents.")
(defark have-10 nickel have-15 "Clunk!")
(defark have-10 dime have-20 "Clink!")
(defark have-10 quarter have-25 "Dime change.")
(defark have-10 coint-return start "Returned ten cents.")
(defark have-15 nickel have-20 "Clunk!")
(defark have-15 dime have-25 "Nickel change.")
(defark have-15 quarter have-25 "Nickel and dime change.")
(defark have-15 gum-button end "Deliver gum.")
(defark have-15 coin-return start "Returned fifteen cents.")
(defark have-20 nickel have-25 "Clunk.")
(defark have-20 dime have-25 "Nickel change.")
(defark have-20 quarter have-25 "2 dimes change.")
(defark have-20 gum-button end "Deliver gum, nickel change.")
(defark have-20 mint-button end "Deliver mints.")
(defark have-20 coin-return start "Returned twenty cents.")
(defark have-25 nickel have-25 "Nickel returned.")
(defark have-25 dime have-25 "Dime returned.")
(defark have-25 quarter have-25 "Quarter returned.")
(defark have-25 gum-button end "Deliver gum, dime change.")
(defark have-25 mint-button end "Deliver gum, nickel change.")
(defark have-25 chocolate-bar-button end "Deliver chocolate bar.")
(defark have-25 coin-return start "Returned twenty five cents.")

;(fsm) ;to run; inputs: dime, nickel, ..., coin-return, mint-button ... depending on the state you are in

;;Keyboard ex 4.11

; "Our finite state machine simulator is called an ‘‘interpreter’’: It operates by
; interpreting the node and arc data structures as a machine description. A faster
; way to simulate a finite state machine is to write a specialized function for
; each node. [...]
; This approach is faster because we don’t have to call ASSOC or FINDNODE.
; In fact, we don’t reference the node and arc data structures as all. The
; speedup may be important if we are simulating a complex machine with many
; states, such as a piece of computer circuitry."
; This version won't be asking for directions interactively like the above

(defun compile-ark (ark)
"takes an ark as input and returns a COND clause (see below)"
	(let  ((to (noda-name (ark-to ark)))
		   (label (ark-label ark))
		   (action (ark-action ark)))
	`((equal this-input ',label)
	  (format t "~&~A" ,action)
	  (,to (rest input-syms)))))

; (COMPILE-ARC (FIRST *ARCS*)) ;used to test
; expected return:
; ((equal this-input ’nickel)
; (format t "~&~A" "Clunk!")
; (have-5 (rest input-syms)))

(defun compile-noda (noda)
"Takes a noda as input and returns a DEFUN expression for that noda."
	(let ((outputs (noda-outputs noda))
		  (name (noda-name noda)))
		`(defun ,name (input-syms 
						&aux (this-input (first input-syms)))
			(cond ((null input-syms) ',name)
				  ,@(mapcar #'(lambda (this) (compile-ark this)) outputs) 
				  (t (error "No ark from ~A with label ~A."
							',name this-input))))))

; (COMPILE-NODE(FIND-NODE 'START)) ;used to test
; expected return (depending on number of arcs outputs defined for a given node)
; (defun start (input-syms
; 	&aux (this-input (first syms)))
; (cond ((null input-syms) ’start)
; 		((equal this-input ’nickel)
; 		 (format t "~&~A" "Clunk!")
; 		 (have-5 (rest input-syms)))
; 		((equal this-input ’dime)
; 		 (format t "~&~A" "Clink!")
; 		 (have-10 (rest input-syms)))
; 		((equal this-input ’coin-return)
; 	 	 (format t "~&~A" "Nothing to return.")
; 		 (start (rest input-syms)))
; 		(t (error "No arc from ~A with label ~A."
; 			’start this-input))))

(defmacro compile-machine ()
"expands into a PROGN containing a DEFUN for each noda in *NODAS*."
	`(progn ,@(mapcar #'compile-noda *nodas*)
		'(compiled ,@*nodas*)))
		
(compile-machine) "to compile the vending machine."
;(start '(dime dime dime gum-button)) ;example how to run