; Source: COMMON LISP: A Gentle Introduction to Symbolic Computation by David S. Touretzky

; Chapter 13 - Arrays, Hash Tables, And Property Lists - Cryptogram using lisp hash tables

; Hash Table keyboard ex.13.9

; A cryptogram is a type of puzzle that requires the solver to decode a message.
; The code is known as a substitution cipher because it consists of substituting
; one letter for another throughout the message.
; The purpose of this keyboard exercise is not to solve cryptograms by hand,
; but to write a program to help you solve them.
; command "undo" undos the letter in the cypher

(setf *crypto-text*
	'("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
		"enlpo pib slafml pvv bfwkj"))

		
(setf *encipher-table* (make-hash-table))
(setf *decipher-table* (make-hash-table))
"We will use *DECIPHER-TABLE* to print out the deciphered cryptogram. 
We need *ENCIPHER-TABLE* to check for two letters being deciphered to the same thing,"

(defun make-substitution (code letter)
"Takes two character objects as input and stores the appropriate entries in *DECIPHERTABLE*
and *ENCIPHER-TABLE* so that the first letter deciphers to the second and the second letter enciphers to the first."
	(setf (gethash letter *encipher-table*) code)
	(setf (gethash code *decipher-table*) letter))

(defun undo-substitution (code)
"Takes one letter as input. It should set the *DECIPHER-TABLE* entry of that letter,
and the *ENCIPHER-TABLE* entry of the letter it deciphered to, to NIL."
	(setf (gethash (gethash code *decipher-table*) *encipher-table*) nil)
	(setf (gethash code *decipher-table*) nil))

(defun clear ()
"Clears the two hash tables used in this problem."
	(clrhash *decipher-table*) 
	(clrhash *encipher-table*))
	
(defun decipher-string (encoded-string)
"Takes a single encoded string as input and returns a new, partially decoded string."
	(let ((len (length encoded-string)))
		(setf decoded-string (make-string len :initial-element #\Space))
		(dotimes (i len decoded-string)
			(let ((found (gethash (aref encoded-string i) *decipher-table*)))
				(unless (null found) 
					(setf (aref decoded-string i) found))))))

(defun show-line (crypto-line)
"Displays one line of cryptogram text, with the deciphered text displayed beneath it."
	(format t "~&~A" crypto-line)
	(format t "~&~A" (decipher-string crypto-line)))

(defun show-text (list-of-crypto-lines)
"Takes a cryptogram (list of strings) as input and displays the lines"
	(format t "~&--------------------")
	(dolist (cline list-of-crypto-lines)
		(show-line cline)
		(format t "~%~%"))
	(format t "--------------------"))

(defun get-first-char (x)
"Returns the first character in the lowercase printed representation of an object"
	(char-downcase 
		(char (format nil "~A" x) 0)))
		
(defun read-letter ()
"reads an object from the keyboard."
	(let ((object (read)))
		(if (or (eq object 'undo) (eq object 'end))
			object
			(get-first-char object))))
			
(defun sub-letter (char-obj)
"Takes a character object as input. If that character has been deciphered already, 
SUB-LETTER should print an error message"
	(let ((decif (gethash char-obj *decipher-table*)))
		(cond (decif (format t "'~A' has already been deciphered as '~A'!" char-obj decif))
			  (t 
				(format t "What does '~A' should decipher to? " char-obj)	
				(let ((new (get-first-char (read))))
					(if (and (not (symbolp new)) (not (gethash new *encipher-table*)))
						;(format t "~%DEBUG code=~A letter=~~A" char-obj new)
						(make-substitution char-obj new)
						(format t "But '~A' already deciphers to '~A'!" char-obj new)))))))

(defun undo-letter ()
"Asks the question and reads in a character to undo. If that character has been deciphered,
UNDO-LETTER should call UNDO-SUBSTITUTION on the letter"
	(format t "Undo which letter? ")
	(let* ((code (get-first-char (read)))
		   (decif (gethash code *decipher-table*)))
		  (cond (decif (undo-substitution code))
				(t (format t "'~A' has not yet been deciphered!" code)))))

(defun solve (cgram)
"Main function to SOLVE which takes a cryptogram as input.
SOLVE should perform the following loop:"
	(do ((new nil))
		((eq new 'end) (return-from solve 'bye))
		(show-text cgram)
		(format t "~%Substitute which letter? ")
		(let ((new (read-letter)))
			(cond ((not (symbolp new)) (sub-letter new))
				  ((eq new 'undo) (undo-letter))
				  ((eq new 't) 't)
				  ((eq new 'end) (return-from solve 'bye))
		    	  (t (format t "I can't process '~A'..." new))))))
			

; (solve *crypto-text*) ;use to run and start deciphering the text
; ;Hint: P deciphers to A, and Z deciphers to I.

