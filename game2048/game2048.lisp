;;;; game2048.lisp

(in-package #:game2048)

;;; "game2048" goes here. Hacks and glory await!

(defparameter *board-size* 4)
(defparameter *num-init-tile* 2)
(defparameter *win-threshold* 2048)

;; Helper functions and macros to make 2d array processing easier
(defconstant up '(-1 0))
(defconstant down '(1 0))
(defconstant left '(0 -1))
(defconstant right '(0 1))

(defun vec-plus (a b)
  (mapcar #'+ a b))

(defun vec-neg (v)
  (mapcar #'- v))

(defun vec-normal (v)
  (list (abs (second v)) (abs (first v))))

(defun vec-start (v)
  (if (< (apply '+ v) 0) (list 0 0) (mapcar #'* v (list (1- *board-size*) (1- *board-size*)))))

; not really needed (array has a builtin function), but then...
(defun vec-bound (v)
  (let ((x (first v))
	(y (second v)))
    (and (and (<= 0 x) (< x *board-size*))
	 (and (<= 0 y) (< y *board-size*)))))

(defmacro vec-aref (array pos)
  `(aref ,array (first ,pos) (second ,pos)))


;; Collection of helper functions
(defun have-mergeable-cell (board)
  (loop for x from 0 to (1- *board-size*) thereis
       (loop for y from 0 to (1- *board-size*) thereis
	  (let* ((v (list x y))
		 (vr (vec-plus v right))
		 (vd (vec-plus v down)))
	    (and (not (zerop (vec-aref board v)))
		 (or (and (vec-bound vr) (= (vec-aref board vr) (vec-aref board v)))
		     (and (vec-bound vd) (= (vec-aref board vd) (vec-aref board v)))))))))

(defun max-cell (board)
  (loop for i from 0 to (1- (array-total-size board))
     maximizing (row-major-aref board i)))

(defun findall-empty-slots (board)
  (loop for i from 0 to (1- (array-total-size board))
     when (zerop (row-major-aref board i)) collect i))

(defun add-tile (board)
  (let* ((slots (findall-empty-slots board))
	 (cnt (length slots)))
    (setf (row-major-aref board (nth (random cnt) slots)) (* 2 (1+ (random 2)))))) 


;; Core functions: start a new board, and update a board based on a move
(defun init-board ()
  (let ((board (make-array `(,*board-size* ,*board-size*)
			   :element-type 'integer
			   :initial-element 0)))
    (dotimes (i *num-init-tile*)
      (add-tile board))
    board))

(defun update-board (board dir)
  (let ((changed nil))
    (loop repeat *board-size*
       for p = (vec-start dir) then (vec-plus p (vec-normal dir)) with d = (vec-neg dir) do
	 (loop for r = p then (vec-plus r d) with w = p
	    while (vec-bound w) do
	      (if (vec-bound r)
		  (if (not (zerop (vec-aref board r)))
		      (progn (setf (vec-aref board w) (vec-aref board r))
			     (if (not (equal w r)) (setq changed t)) ;Tile loc diff -> changed 
			     (setf w (vec-plus w d))))
		  (progn (setf (vec-aref board w) 0)
			 (setf w (vec-plus w d)))))
	 (loop for w = p then (vec-plus w d) with r = p
	    while (vec-bound w) do
	      (if (and (vec-bound r) (not (zerop (vec-aref board r))))
		  (if (and (vec-bound (vec-plus r d))
			   (= (vec-aref board r) (vec-aref board (vec-plus r d))))
		      (progn (setf (vec-aref board w) (* 2 (vec-aref board r)))
			     (setf r (vec-plus (vec-plus r d) d))
			     (setq changed t)) ;Merged cell -> changed
		      (progn (setf (vec-aref board w) (vec-aref board r))
			     (setf r (vec-plus r d))))
		  (setf (vec-aref board w) 0))))
  changed))


;; Functions to test win/loss
(defun iswon (board)
  (>= (max-cell board) *win-threshold*))

(defun islost (board)
  (and (= (length (findall-empty-slots board)) 0)
       (not (have-mergeable-cell board))))


;; Function to printout the board and parse input
(defun print-board (board)
  (let ((nd (1+ (floor (log (max-cell board) 10)))))
    (format t "+~v@{~v,,,vA~3:*+~}~%" *board-size* nd #\- #\-)
    (loop for i from 0 to (1- *board-size*) do
	 (let ((row (make-array *board-size*
				:displaced-to board
				:displaced-index-offset (* i *board-size*)))
	       (nds (write-to-string nd)))
	   (format t (concatenate 'string
				  "|~{~[~" nds "@T~:;~:*~" nds "@a~]|~}~%") (coerce row 'list))
	   (format t "+~v@{~v,,,vA~3:*+~}~%" *board-size* nd #\- #\-)))))

(defun parse-move (str)
  (cond ((string-equal str "u") up)
	((string-equal str "d") down)
	((string-equal str "l") left)
	((string-equal str "r") right)
	(t nil)))


;; Main function for a game
(defun play-game ()
  (format t "Game 2048~%")
  (let ((myboard (init-board))
	(mv nil))
    (loop
       (print-board myboard)
       (if (iswon myboard)
	   (progn (format t "You win!~%") (return)))
       (if (islost myboard)
	   (progn (format t "You lost!~%") (return)))
       (loop
	  (princ "Your move (u,d,l,r): ")
	  (finish-output nil)
	  (setq mv (parse-move (read-line)))
	  (if mv (return) (format t "Unrecognized input, try again.~%")))
       (if (update-board myboard mv)
	   (add-tile myboard)))))

