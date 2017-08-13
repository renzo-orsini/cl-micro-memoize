

(defmacro defmem(name args &body body)
  (let ((ll (length args))
	(val-name (gensym))
	(val-val (gensym))
	(t-name (gensym))
	(res-name (gensym))
	(pre-name (gensym)))
    (cond ((= ll 0)                      ; no arguments
	   `(let ((,val-name ',val-val))
	      (defun ,name ,args
		(if (eq ,val-name ',val-val)
		    (setf ,val-name (progn ,@body))
		    ,val-name))))
	  ((= ll 1)                            ; one argument	       
	   `(let ((,t-name (make-hash-table :test 'equal)))
	      (defun ,name (,@args)
		(multiple-value-bind (,res-name ,pre-name)
		    (gethash ,@args ,t-name)
		  (if ,pre-name
		      ,res-name
		      (setf (gethash ,@args ,t-name)
			    (progn ,@body)))))))
	  (t                                  ; multiple arguments
	   `(let ((,t-name (make-hash-table :test 'equal)))
	      (defun ,name ,args
		(multiple-value-bind (,res-name ,pre-name)
		    (gethash (list ,@args) ,t-name)
		  (if ,pre-name
		      ,res-name
		      (setf (gethash (list ,@args) ,t-name)
			    (progn ,@body))))))))))
