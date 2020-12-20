; Source: COMMON LISP: A Gentle Introduction to Symbolic Computation by David S. Touretzky

; Chapter 10 - Assignment - playing tic tac toe with computer

;;Example ex10.8 - TIC TAC TOE case study

; "This keyboard exercise requires you to add some additional strategies to the tic-tac-toe playing program
; The first strategy we’ll consider is called the squeeze play, which can be made using either of the two diagonal triplets
; A second offensive strategy we want to guard against is called ‘‘two on one.’’ it can be set up using either diagonal triplet"


(defun make-board ()
"Represent a board as a list consisting of the symbol BOARD followed by nine numbers that describe the contents of each position. 
A zero means the position is empty; a one means it is filled by an O; a ten means it is filled by an X."
	(list 'board 0 0 0 0 0 0 0 0 0))
	;(list 'board 1 0 0 0 1 0 0 0 10))

	
(defun convert-to-letter (v)
"converts a zero, one, or ten to a space, an O, or an X, respectively"
	(cond ((equal v 1) "O")
   		((equal v 10) "X")
		(t " ")))

(defun print-row (x y z)
"prints out one row of the board"
	(format t "~& ~A | ~A | ~A"
		(convert-to-letter x)
		(convert-to-letter y)
		(convert-to-letter z)))
		
(defun print-board (board)
"prints board representation using print-row"
	(format t "~%")
	(print-row (nth 1 board) (nth 2 board) (nth 3 board))
	(format t "~& -----------")
	(print-row (nth 4 board) (nth 5 board) (nth 6 board))
	(format t "~& -----------")
	(print-row (nth 7 board) (nth 8 board) (nth 9 board))
	(format t "~%~%"))
	
(defun make-move (player pos board)
"make a move by destructively changing one of the board positions from a zero to a one (for O) or a ten (for X)."
	(setf (nth pos board) player)
	board)

; "We will store a list of all eight triplets in a global variable *TRIPLETS*."
(setf *triplets*
	'((1 2 3) (4 5 6) (7 8 9) 	;Horizontal triplets.
	  (1 4 7) (2 5 8) (3 6 9) 	;Vertical triplets.
	  (1 5 9) (3 5 7)        ))	;Diagonal triplets.


(defun sum-triplet (board triplet)
"return the sum of the numbers in the board positions specified by that triplet. For example, the right diagonal triplet is (3 5 7).
The sum of elements three, five, and seven of board B is eleven, indicating that there is one O, one X, and one blank (in some
unspecified order) on that diagonal."
	(+  (nth (first triplet) board)
		(nth (second triplet) board)
		(nth (third triplet) board)))

(defun compute-sums (board)
"returns a list of all eight sums"
	(mapcar #'(lambda (triplet) (sum-triplet board triplet))
			*triplets*))
			
(defun winner-p (board)
"if player O ever gets three in a row, one of the eight sums will
be three. Similarly, if player X manages to get three in a row, one of the eight
sums will be 30."
	(let ((sums (compute-sums board)))
		(or (member (* 3 *computer*) sums)
			(member (* 3 *opponent*) sums))))

(defun play-one-game ()
"offers the user the choice to go first, and then calls either COMPUTERMOVE
or OPPONENT-MOVE as appropriate, passing a new, empty board as input."
	(setf *number-of-moves* 0)
	(if (y-or-n-p "Would you like to go first? ")
		(opponent-move (make-board))
		(computer-move (make-board))))

(defun opponent-move (board)
"asks the opponent to type in a move and checks that the move is legal. It then updates the board and calls COMPUTER-MOVE."
	(incf *number-of-moves*)
	(let* ((pos (read-a-legal-move board))
			(new-board (make-move
					*opponent*
					pos
					board)))
		(print-board new-board)
		(cond 	((winner-p new-board)
					(format t "~&You win!"))
				((board-full-p new-board)
					(format t "~&Tie game."))
				(t (computer-move new-board)))))

(defun read-a-legal-move (board)
"legal move is an integer between one and nine such that the corresponding board position is empty."
	(format t "~&Your move: ")
	(let ((pos (read)))
		(cond 	((not (and (integerp pos) (<= 1 pos 9)))
					(format t "~&Invalid input.")
					(read-a-legal-move board))
				((not (zerop (nth pos board)))
					(format t "~&That space is already occupied.")
					(read-a-legal-move board))
				(t pos))))

(defun board-full-p (board)
"test if there are no more empty spaces left on the board"
	(not (member 0 board)))

(defun computer-move (board)
"similar to OPPONENT-MOVE, except the player is X instead of O, and instead of reading a move from the
keyboard, we will call CHOOSE-BEST-MOVE."
	(incf *number-of-moves*)
	(let* 	((best-move (choose-best-move board))
			(pos (first best-move))
			(strategy (second best-move))
			(new-board (make-move *computer* pos board)))
		(format t "~&My move: ~S" pos)
		(format t "~&My strategy: ~A~%" strategy)
		(print-board new-board)
		(cond 	((winner-p new-board)
					(format t "~&I win!"))
				((board-full-p new-board)
					(format t "~&Tie game."))
				(t (opponent-move new-board)))))
				

;(defun choose-best-move (board) ;First version.
; "move strategy selection"
;	 (random-move-strategy board))

;(defun choose-best-move (board) ;Second version.
; "prefer these two more clever strategies to
; the random move strategy."
;	(or (make-three-in-a-row board)
;		(block-opponent-win board)
;		(random-move-strategy board)))


(defun random-move-strategy (board)
	(list (pick-random-empty-position board) "random move"))

(defun pick-random-empty-position (board)
"picks a random number from one to nine. If that board position is
empty, it returns that move. Otherwise, it calls itself recursively to try another
random number."
	(let ((pos (+ 1 (random 9))))
		(if (zerop (nth pos board))
			pos
			(pick-random-empty-position board))))
			

(defun make-three-in-a-row (board)
"we can program it to look for two-in-a-row
situations. If there are two Xs in a row, it should fill in the third X to win the game."
	(let ((pos (win-or-block board (* 2 *computer*))))
		(and pos (list pos "make three in a row"))))


(defun block-opponent-win (board)
	(let ((pos (win-or-block board (* 2 *opponent*))))
		(and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
	(let ((triplet (find-if #'(lambda (trip) 
									(equal (sum-triplet board trip)
									target-sum))
							*triplets*)))
		(when triplet
			(find-empty-position board triplet))))

(defun find-empty-position (board squares)
	(find-if #'(lambda (pos) (zerop (nth pos board)))
			squares))


;;Exercise 10.8.a

(setf *corners*
; to hold a list of the four corner positions."
	'(1 3 7 9))

(setf *sides*
; to hold a list of the four side squares"	
	'(2 4 6 8))

	
;;Exercise 10.8.b
(defun block-squeeze-play (board)
"checks the diagonals for an O-X-O pattern and defends by suggesting a side
square as the best move."
	(when (equal *number-of-moves* 4)
		(when (or (and (equal (nth 1 board) *opponent*) 
						(equal (nth 5 board) *computer*) 
						(equal (nth 9 board) *opponent*))
				   (and (equal (nth 3 board) *opponent*) 
						(equal (nth 5 board) *computer*) 
						(equal (nth 7 board) *opponent*)))					
			  (list (find-empty-position board *sides*) "block squeeze play"))))

;;Exercise 10.8.c
(defun block-two-on-one (board)
"checks the diagonals for an O-O-X or X-O-O pattern and defends by suggesting a corner
as the best move."
	(when (equal *number-of-moves* 4)
		(when (or (and (equal (nth 1 board) *opponent*) 
						(equal (nth 5 board) *opponent*) 
						(equal (nth 9 board) *computer*))
				  (and (equal (nth 9 board) *opponent*) 
						(equal (nth 5 board) *opponent*) 
						(equal (nth 1 board) *computer*))
				  (and (equal (nth 3 board) *computer*) 
						(equal (nth 5 board) *opponent*) 
						(equal (nth 7 board) *opponent*))					
				  (and (equal (nth 7 board) *computer*) 
						(equal (nth 5 board) *opponent*) 
						(equal (nth 3 board) *opponent*)))					
			  (list (find-empty-position board *corners*) "block two-on-one"))))

;;Exercise 10.8.d
(defun choose-best-move (board) ;Third version.
"tries the above two defensive strategies before choosing a move at random."
	(format t "~&Move number: ~S~%" *number-of-moves*)
	(or (make-three-in-a-row board)
		(block-opponent-win board)
		(block-squeeze-play board)
		(block-two-on-one board)
		(attack-squeeze-play-1 board) ;10.8.e
		(attack-squeeze-play-2 board) ;10.8.e
		(attack-two-on-one-1 board)   ;10.8.e
		(attack-two-on-one-2 board)   ;10.8.e
		(random-move-strategy board)))


;;Exercise 10.8.e
"opportunity for the computer to set up a squeeze play or two-on-one situation to trap the opponent"

(setf *diagonal\* 
;"to hold one of diagonals"
	'(1 5 9))

(setf *diagonal/* 
;"to hold the second of diagonals"
	'(3 5 7))

(defun attack-squeeze-play-1 (board) 
		(when (or 	(and 	(equal (nth 5 board) *opponent*) 
							(equal (nth 9 board) *computer*)
							(equal *number-of-moves* 3))
					(and 	(equal (nth 5 board) *opponent*) 
							(equal (nth 1 board) *computer*)
							(equal *number-of-moves* 3)))
			  (list (find-empty-position board *diagonal\*) "attack squeeze play")))

(defun attack-squeeze-play-2 (board) 
		(when (or 	(and 	(equal (nth 5 board) *opponent*) 
							(equal (nth 7 board) *computer*)
							(equal *number-of-moves* 3))
					(and 	(equal (nth 5 board) *opponent*) 
							(equal (nth 3 board) *computer*)
							(equal *number-of-moves* 3)))
			  (list (find-empty-position board *diagonal/*) "attack squeeze play")))


(defun attack-two-on-one-1 (board) 
		(when (or 	(and 	(equal (nth 1 board) *opponent*) 
							(equal (nth 9 board) *computer*)
							(equal *number-of-moves* 3))
					(and 	(equal (nth 9 board) *opponent*) 
							(equal (nth 1 board) *computer*)
							(equal *number-of-moves* 3)))
			  (list (find-empty-position board *diagonal\*) "attack two-on-one")))

(defun attack-two-on-one-2 (board) 
		(when (or 	(and 	(equal (nth 3 board) *opponent*) 
							(equal (nth 7 board) *computer*)
							(equal *number-of-moves* 3))
					(and 	(equal (nth 7 board) *opponent*) 
							(equal (nth 3 board) *computer*)
							(equal *number-of-moves* 3)))
			  (list (find-empty-position board *diagonal/*) "attack two-on-one")))


; testing:
;(setf b (make-board))	
;(make-move *opponent* 3 b)
;(make-move *computer* 5 b)

(setf *number-of-moves* 0)
(setf *computer* 10)
(setf *opponent* 1)
;(play-one-game) ;to run 

