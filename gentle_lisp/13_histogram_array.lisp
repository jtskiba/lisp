; Source: COMMON LISP: A Gentle Introduction to Symbolic Computation by David S. Touretzky

; Chapter 13 - Arrays, Hash Tables, And Property Lists - Histogram using lisp arrays

; Array keyboard ex.13.8

; "Let’s find out how random your Lisp’s random number generator is. In this
; exercise we will produce a histogram plot of 200 random values between zero
; and ten. We will use an array to keep track of how many times we encounter
; each value."


(setf *print-array* t)

(defun new-histogram (bins)
"Holds the array of counts, and a global variable *TOTAL-POINTS*
that holds the number of points recorded so far"
	(setf *hist-array* (make-array bins :initial-element 0))
	(setf *total-points* 0))

(defun record-value (number)
"Takes a number as input. If the number is between zero and ten, it should increment the
appropriate element of the array, and also update *TOTALPOINTS*."
		(cond ((and (>= number 0) (< number (length *hist-array*)))
			   (incf (aref *hist-array* number))
			   (incf *total-points*))
			   (t nil)))
			   
(defun print-hist-line (number)
"Takes a value from zero to ten as input, looks up that value in the array, and prints the
corresponding line of the histogram."
	(let ((count (aref *hist-array* number)))
		(cond ((and (>= number 0) (< number (length *hist-array*)))
				(format t "~&~2@S [~3@S]  " number count) 
				(dotimes (i count)
					(format t "*")))
			  (t nil))))

(defun print-histogram ()
	(dotimes (i (length *hist-array*))
		(print-hist-line i))
	(format t "~&    ~3@S  total" *total-points*))

; ;for testing:	
; (new-histogram 11)
; (record-value 3)
; (record-value 3)
; (record-value 3)
; (record-value 3)
; (record-value 3)
; (record-value 1)
; (record-value 1)
; (record-value 7)
; (record-value 7)
; (record-value 7)

; to run type to the following to repl e.g. 
; > (new-histogram 11) ;Eleven bins: 0 to 10.
; > (dotimes (i 200) (record-value (random 11)))
; > (print-histogram)


(new-histogram 11)
(dotimes (i 200)
	(record-value (random 11)))
; (print-histogram) ;enter in repl

