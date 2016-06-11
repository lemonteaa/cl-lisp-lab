(defmacro for (listspec exp)
  (cond ((and (= (length listspec) 3)
	      (symbolp (car listspec))
	      (eq (cadr listspec) ':in))
	 `(mapcar (lambda (,(car listspec))
		    ,exp)
		  ,(caddr listspec)))
	(t (error "Ill-formed: %s" `(for ,listspec ,exp)))))

