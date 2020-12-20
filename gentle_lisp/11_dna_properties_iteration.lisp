; Source: COMMON LISP: A Gentle Introduction to Symbolic Computation by David S. Touretzky

; Chapter 11 - Iteration and Block Structure - exploring DNA properties with lisp iterations

; Iterations keyboard ex.11.22 - DNA properties

; "In this keyboard exercise we will explore some properties of single- and double-stranded DNA.
; A strand of DNA is very much like a chain of cons cells; the elements of the chain are of four types, 
; corresponding to the four bases adenine, thymine, guanine, and cytosine. We will represent a
; strand of DNA by a list of bases.[...]
; Each of the four bases has a complement with which it can form a pair.
; Adenine pairs with thymine, while guanine pairs with cytosine. Two single
; strands of DNA can combine to form double-stranded DNA (whose shape is
; the famous ‘‘double helix’’) when each of their corresponding bases are complementary."

(setq *complements* '((A T) (T A) (G C) (C G)))

(defun complement-base (base)
"takes a base as input and returns the matching complementary base."
	(cadr (assoc base *complements*)))

(defun complement-strand (lst)
"Returns the complementary strand of a sequence of single-stranded DNA."
	(mapcar #'complement-base lst))

(defun make-double (lst)
"Takes a single strand of DNA as input and returns a double-stranded version"
	(mapcar #'(lambda (x y) (list x y)) lst (complement-strand lst)))
	
(defun flatten (L)
"Converts a list to single level."
    (if (null L)
        nil
        (if (atom (first L))
            (cons (first L) (flatten (rest L)))
            (append (flatten (first L)) (flatten (rest L))))))

(defun count-x (x lst)
	(length (remove-if-not #'(lambda (i) (equal x i)) lst)))

(defun count-bases (lst)
"Counts the number of bases of each type in a DNA strand, and returns the result as a table. Your function should work
for both single- and double-stranded DNA. 
Example: (COUNT-BASES '((G C) (A T) (T A) (T A) (C G))) should return ((A 3) (T 3) (G 2) (C 2))."
	(let ((flat (flatten lst))
		  (bases '(a t g c)))
		 (mapcar #'(lambda (x) (list x (count-x x flat))) bases)))

(defun prefixp (first second)
"Predicate that returns T if one strand of DNA is a prefix of another."
	(if (null first)
		't
		(if (equal (car first) (car second))
			(prefixp (cdr first) (cdr second))
			nil)))
		
(defun appearsp (first second)
"Predicate returns T if one DNA strand appears anywhere within another."
	(if (< (length second) (length first))
		nil
		(if (prefixp first second)
			't
			(appearsp first (cdr second)))))
		
(defun coverp (first second)
"Predicate returns T if its first input, repeated some number of times, matches all of its second input."
	(let ((len-1 (length first))
		  (len-2 (length second)))
		 (unless (integerp (/ len-2 len-1))
			(return-from coverp nil))
		 (dotimes (i len-2 't)
			(if (not (equal (nth (mod i len-1) first) (nth i second)))
				(return nil)))))

(defun prefix (number lst)
"Returns the leftmost N number of bases of a DNA strand."
	(if (> number (length lst))
		lst
		(let ((result nil))
		 (dotimes (i number (my-reverse result))
			(setf result (cons (nth i lst) result))))))
			 
(defun kernel (L)
"some naturally occurring DNA strands consist of many repetitions of a short ‘kernel’ sequence.
Returns the shortest prefix of a DNA strand that can be repeated to cover the strand.
Example: (KERNEL '(A G C A G C A G C)) should return (A G C)."
	(dotimes (i (length L) nil)
		(let ((pf (prefix (+ i 1) L)))
			(if (coverp pf L)
				(return pf)))))

(defun print-x-times (x this)
	(format t "~&")
	(dotimes (i x)
		(format t "~A " this)))

(defun print-letters (these)
	(format t "~&")
	(dolist (elt these)
		(format t "    ~A     " elt)))

(defun draw-dna (L)
"Takes a single-stranded DNA sequence as input and draws it along with its complementary strand"
	(let ((len (length L)))
		(print-x-times len "_ _ _ _ _")
		(print-x-times len "    !    ")
		(print-letters L)
		(print-x-times len "    .    ")
		(print-x-times len "    .    ")
		(print-letters (complement-strand L))
		(print-x-times len "    !    ")
		(print-x-times len "_ _ _ _ _")))
		
;(draw-dna '(A G G T C A T T G)) ;example to draw dna double helix