(defun combi-enum (init-s state-update ava-moves completed action)
  (labels ((bt (s x)
	     (if (funcall completed s) (funcall action x)
		 (let ((ms (funcall ava-moves s)))
		   (loop for m in ms do
			(bt (funcall state-update s m) (cons m x)))))))
    (bt init-s nil)))

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

;; some example as tests

; e.g. 1: n choose k
;(combi-enum '(0 0) (lambda (s m) (list m (+ (second s) 1))) (lambda (s) (range 10 :min (+ (first s) 1))) (lambda (s) (= (second s) 4)) (lambda (x) (print x)))

; e.g. 2: queens problem
(defclass board-state ()
  ((board :initform nil :reader getboard :writer setboard)
   (k :initform 0 :reader getk :writer setk)
   (n :initform 8 :initarg :size :reader getn)))

(defmethod initialize-instance :after ((it board-state) &key)
  (setboard (make-array (list (getn it) (getn it)) :initial-element nil) it))

(defmethod board-completed ((it board-state))
  (= (getk it) (getn it)))

(defmethod board-moves ((it board-state))
  (loop for i from 0 to (- (getn it) 1)
     when (not (aref (getboard it) (getk it) i))
     collect (list (getk it) i)))

(defmethod board-update ((it board-state) mov)
  (let ((it-p (make-instance 'board-state :size (getn it))))
    (loop for x from 0 to (- (getn it) 1) do
	 (loop for y from 0 to (- (getn it) 1) when
	      (or (aref (getboard it) x y)
		  (= x (first mov))
		  (= y (second mov))
		  (= (abs (- x (first mov))) (abs (- y (second mov)))))
	      do (setf (aref (getboard it-p) x y) t)))
    (setk (+ (getk it) 1) it-p)
    it-p))

(defun queen-solve (n)
  (let ((empty-board (make-instance 'board-state :size n))
	(sol-lst nil))
    (combi-enum empty-board #'board-update #'board-moves #'board-completed (lambda (x) (setf sol-lst (cons x sol-lst))))
    sol-lst))
