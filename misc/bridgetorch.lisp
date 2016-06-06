(defun solve-internal (tlst t2 t1 n)
  (case n
    (2 '((1 2)))
    (3 '((1 3) (1) (1 2)))
    (otherwise (let ((tn (car tlst))
		     (tn-1 (car (cdr tlst)))
		     (trst (cdr (cdr tlst))))
		 (if (< (+ t1 (* 2 t2) tn) (+ (* 2 t1) tn-1 tn))
		     (list* '(1 2) '(1) `(,(- n 1) ,n) '(2) (solve-internal trst t2 t1 (- n 2)))
		     (list* `(1 ,n) '(1) `(1 ,(- n 1)) '(1) (solve-internal trst t2 t1 (- n 2))))))))

(defun solve (tlst)
  (let* ((n (length tlst))
	 (ts (sort tlst #'<))
	 (t1 (first ts))
	 (t2 (second ts)))
    (solve-internal (reverse ts) t2 t1 n)))

(defun comma-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defclass bridgetorch-game ()
  ((time-list :initform nil :initarg :tlst)
   (ppl-state-l :initform nil)
   (torch-state-l :reader torch-on-left :initform t)
   (time-used :reader time-used :initform 0)
   (numl :initform 0)))

(defmethod initialize-instance :after ((it bridgetorch-game) &key)
  (let* ((n (length (slot-value it 'time-list)))
	 (tsorted (sort (slot-value it 'time-list) #'<)))
    (progn (setf (slot-value it 'time-list) (make-array n :initial-contents tsorted))
	   (setf (slot-value it 'ppl-state-l) (make-array n :initial-element t))
	   (setf (slot-value it 'numl) n))))

(defmethod ppl-left ((it bridgetorch-game))
  (loop for i upfrom 0
        for x across (slot-value it 'ppl-state-l)
        when x collect (aref (slot-value it 'time-list) i)))

(defmethod ppl-right ((it bridgetorch-game))
  (loop for i upfrom 0
        for x across (slot-value it 'ppl-state-l)
        when (not x) collect (aref (slot-value it 'time-list) i)))

(defmethod isdone ((it bridgetorch-game))
  (= (slot-value it 'numl) 0))

(defmethod valid-move ((it bridgetorch-game) m)
  (let ((k (length m))
	(a (first m))
	(b (second m)))
    (if (torch-on-left it)
	(and (<= k 2) (aref (slot-value it 'ppl-state-l) (- a 1))
	              (aref (slot-value it 'ppl-state-l) (- b 1)))
	(and (<= k 1) (not (aref (slot-value it 'ppl-state-l) (- a 1)))))))

(defmethod move ((it bridgetorch-game) m)
  (when (and (>= (length m) 1) (valid-move it m))
    (let ((tt 0))
      (dolist (i m)
	(setq tt (max tt (aref (slot-value it 'time-list) (- i 1))))
	(if (torch-on-left it)
	    (progn (decf (slot-value it 'numl) 1)
		   (setf (aref (slot-value it 'ppl-state-l) (- i 1)) nil))
	    (progn (incf (slot-value it 'numl) 1)
		   (setf (aref (slot-value it 'ppl-state-l) (- i 1)) t))))
      (incf (slot-value it 'time-used) tt))
    (if (torch-on-left it)
	(setf (slot-value it 'torch-state-l) nil)
	(setf (slot-value it 'torch-state-l) t))))

(defmethod print-state ((it bridgetorch-game))
  (format t "Bridge status:~%")
  (format t "Left side: ~{~a~^, ~}~%" (ppl-left it))
  (format t "Right side: ~{~a~^, ~}~%" (ppl-right it))
  (format t "The torch is on the ~:[right~;left~] side.~%" (torch-on-left it))
  (format t "(Time used: ~d)~%" (time-used it)))

(defun play-game (tlst)
  (let ((game (make-instance 'bridgetorch-game :tlst tlst))
	(mv nil))
    (loop
       (print-state game)
       (if (isdone game)
	   (progn (format t "Finished! Total time used: ~d~%" (time-used game)) (return)))
       (princ "Your move: ")
       (finish-output nil)
       (setq mv (mapcar #'parse-integer (comma-split (read-line))))
       (setq mv (mapcar #'(lambda (x) (+ 1 (position x (slot-value game 'time-list)))) mv))
       (move game mv))))

(defun ai-play (tlst)
  (let ((game (make-instance 'bridgetorch-game :tlst tlst))
	(sol (solve tlst)))
    (loop
       (print-state game)
       (if (isdone game)
	   (progn (format t "Finished! Total time used: ~d~%" (time-used game)) (return)))
       (princ "Your move: ")
       (format t "~{~a~^, ~}~%"
	       (mapcar #'(lambda (x) (aref (slot-value game 'time-list) (- x 1)))
		       (car sol)))
       (finish-output nil)
       (move game (car sol))
       (setq sol (cdr sol)))))