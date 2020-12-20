; Source: COMMON LISP: A Gentle Introduction to Symbolic Computation by David S. Touretzky

; Chapter 12 - Structures and The Type System - Auto engine diagnostics using discrimination nets (yes/nos)

; Structures keyboard ex.12.4 - auto diagnostics

; In this keyboard exercise we will implement a discrimination net.
; Discrimination nets are networks of yes and no questions used for problemsolving
; tasks, such as diagnosing automotive engine trouble. 

(defstruct node
	(name nil)
	(question nil)
	(yes-case nil)
	(no-case nil))

(setq *node-list* nil)

(defun init ()
	(defvar *node-list* nil))

(defun add-node (nm qn yes no)
	(setf *node-list*
		(cons 
			(make-node 
				:name nm
			    :question qn
			    :yes-case yes
			    :no-case no)
			*node-list*)))
	
(add-node 
	'start
	"Does the engine turn over?"
	'engine-turns-over
	'engine-wont-turn-over)

(add-node 
	'engine-turns-over
	"Will the engine run for any period of time?"	
	'engine-will-run-briefly
	'engine-wont-run)

(add-node 
	'engine-wont-run
	"Is there gas in the tank?"
	'gas-in-tank
	"Fill the tank and try starting the engine again.")

(add-node 
	'engine-wont-turn-over
	"Do you hear any sound when you turn the key?"
	'sound-when-turn-key
	'no-sound-when-turn-key)

(add-node 
	'no-sound-when-turn-key
	"Is the battery voltage low?"
	"Replace the battery"
	'battery-voltage-ok)

(add-node 
	'battery-voltage-ok
	"Are the battery cables dirty or loose?"
	"Clean the cables and tighten the connections."
	'battery-cables-good)

(defun find-node (nm)
"Takes a node name as input and returns the node if it appears in *NODE-LIST*, or NIL if it doesn’t"
	(dolist (nod *node-list*)
		(if (equal (node-name nod) nm)
			(return nod))))

(defun process-node (nd)
"takes a node name as input. If it can’t find the node, it prints a message that the node hasn’t been defined
yet, and returns NIL. Otherwise it asks the user the question associated with that node, and then returns the 
node’s yes action or no action depending on how the user responds."
	(let ((this-node (find-node nd)))
		(cond ((not this-node) (format t "~&Node hasn’t been defined yet"))
			  ('t 
				(if (yes-or-no-p (node-question this-node))
						(node-yes-case this-node)
						(node-no-case this-node))))))

(defun run ()
"Maintains a local variable named CURRENT-NODE, whose initial value is START. It loops, calling
PROCESS-NODE to process the current node, and storing the value returned by PROCESS-NODE back into CURRENT-NODE. 
If the value returned is a string, the function prints the string and stops. If the value returned is NIL, it also stops."
		(do ((current-node 'start))
			((or (not (symbolp current-node)) (not (find-node current-node))) current-node) 
			(setf current-node (process-node current-node))))

(defun read-a-name ()
	(do ((answer nil))
		(nil)
		(format t "~&Please type a node name: ")
		(setf answer (read))
		(if (and (not (find-node answer)) (symbolp answer))
			(return answer))
		(format t "~&Sorry, ~S is either already there or is not a symbol. Try again." answer)))

(defun read-a-question ()
	(do ((answer nil))
		(nil)
		(format t "~&Please type a question for your node: ")
		(setf answer (read))
		(if (stringp answer)
			(return answer))
		(format t "~&Sorry, question ~S is not a string enclosed in double quotes. Try again." answer)))

(defun read-a-yes-no (type) ;type should be 'yes or 'no
	(do ((answer nil))
		(nil)
		(format t "~&Please type ~S case for your node: " type)
		(setf answer (read))
		(if (or (symbolp answer) (stringp answer))
			(return answer))
		(format t "~&Sorry, your ~S case of ~S should be a string or a node's name (symbol). Try again." type answer)))

; interactive function to add a new node.
(defun interactive-add-node ()
"It should prompt the user for the node name, the question, and the yes and no actions.
Remember that the question must be a string, enclosed in double quotes. 
Your function should add the new node to the net."
	(add-node 
		(read-a-name) 
		(read-a-question) 
		(read-a-yes-no 'yes) 
		(read-a-yes-no 'no)))

(init)

; (run) ; this in repl to start diagnostics where you answer yes/no

; (interactive-add-node) ; to add a new node interactively
;    ;Remember that the question must be a string, enclosed in double
;    ;quotes. Your function should add the new node to the net.

; Example of additional node from (h): 
; "If the engine will run briefly but then stalls when it’s cold, it is
; possible that the idle rpm is set too low. Write a new node called
; ENGINE-WILL-RUN-BRIEFLY to inquire whether the engine
; stalls when cold but not when warm. If so, have the net go to
; another node where the user is asked whether the cold idle speed is
; at least 700 rpm. If it’s not, tell the user to adjust the idle speed."