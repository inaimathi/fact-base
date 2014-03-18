(in-package #:fact-base)

(defun make-index-fn (ix-name fst snd)
  (with-gensyms (ix)
    `(defmethod index-by ((lookup (eql ,ix-name)) (state index) a b c)
       (declare (ignorable a b c))
       (let ((,ix (gethash ,ix-name (table state))))
	 ,(if snd
	      `(gethash ,snd (gethash ,fst ,ix))
	      `(gethash ,fst ,ix))))))

(defun make-insert-fn (ix-name fst snd)
  (with-gensyms (lookup state fact ix)
    `(defmethod insert-ix ((,lookup (eql ,ix-name)) (,state index) (,fact list))
       (destructuring-bind (a b c) ,fact
	 (declare (ignorable a b c))
	 (let ((,ix (gethash ,ix-name (table ,state))))
	   ,@(if snd
		 `((unless (gethash ,fst ,ix)
		     (setf (gethash ,fst ,ix) (make-hash-table :test 'equal)))
		   (push ,fact (gethash ,snd (gethash ,fst ,ix))))
		 `((push
		    ,fact
		    (gethash ,fst ,ix)))))))))

(defun make-remove-fn (ix-name fst snd)
  (with-gensyms (lookup state fact ix)
    `(defmethod remove-ix ((,lookup (eql ,ix-name)) (,state index) (,fact list))
       (destructuring-bind (a b c) ,fact
	 (declare (ignorable a b c))
	 (let ((,ix (gethash ,ix-name (table ,state))))
	   (format t "Found index...~%")
	   (awhen (gethash ,fst ,ix)
	     (format t "Found entry '~s'...~%" it)
	     ,@(if snd
		   `((when (gethash ,snd it)
		       (setf (gethash ,snd it) (remove ,fact (gethash ,snd it) :test #'equal)))
		     (unless (car (gethash ,snd it))
		       (remhash ,snd it))
		     (when (= 0 (hash-table-count (gethash ,fst ,ix)))
		       (format t "Checking for empty indexes...~%")
		       (remhash ,fst ,ix)))
		   `((setf (gethash ,fst ,ix) (remove ,fact it :test #'equal))))
	     (format t "Removed entry...~%")))))))

(defmacro define-index (&rest order)
  (let ((ix-name (intern (format nil "~{~a~}" order) :keyword))
	(fst (first order))
	(snd (second order)))
    `(progn 
       ,(make-index-fn ix-name fst snd)
       ,(make-insert-fn ix-name fst snd)
       ,(make-remove-fn ix-name fst snd))))
