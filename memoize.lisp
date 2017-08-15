

(defmacro defmem(name args &body body)
  (let* ((ll (length args))
	 (val-name (gensym))
	 (val-val (gensym))
	 (t-name (gensym))
	 (res-name (gensym))
	 (pre-name (gensym)))
    (multiple-value-bind (decs-docs real-body)
	 (loop for (b . r) on body
	       while (or (and (listp b) (eq (car b) 'declare))
			 (and (stringp b) r))
	       collect b into d-d
	       finally (return (values d-d (or r (list b)))))
      (cond ((= ll 0)                      ; no arguments
	     `(let ((,val-name ',val-val))
		(defun ,name ,args ,@decs-docs
		  (if (eq ,val-name ',val-val)
		      (setf ,val-name (progn ,@real-body))
		      ,val-name))))
	    ((= ll 1)                            ; one argument	       
	     `(let ((,t-name (make-hash-table :test 'equal)))
		(defun ,name (,@args) ,@decs-docs
		  (multiple-value-bind (,res-name ,pre-name)
		      (gethash ,@args ,t-name)
		    (if ,pre-name
			,res-name
			(setf (gethash ,@args ,t-name)
			      (progn ,@real-body)))))))
	    (t                                  ; multiple arguments
	     `(let ((,t-name (make-hash-table :test 'equal)))
		(defun ,name ,args ,@decs-docs
		  (multiple-value-bind (,res-name ,pre-name)
		      (gethash (list ,@args) ,t-name)
		    (if ,pre-name
			,res-name
			(setf (gethash (list ,@args) ,t-name)
			      (progn ,@real-body)))))))))))


;; example:

; (defmem fibo (n) (if (< n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))
