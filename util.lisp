(in-package :fact-base)

(defun temp-file-name ()
  (let* ((f (cl-fad:open-temporary))
	 (fname (file-namestring (pathname f))))
    (close f)
    fname))

(defun make-range-fn (&optional min-time max-time)
  (cond ((and min-time max-time)
	 (lambda (entry) 
	   (local-time:timestamp>= max-time (car entry) min-time)))
	(max-time
	 (lambda (entry)
	   (local-time:timestamp>= max-time (car entry))))
	(min-time
	 (lambda (entry)
	   (local-time:timestamp>= (car entry) min-time)))
	(t (constantly t))))

(defmacro matching? (&rest optima-clauses)
  "Takes an optima match clause. 
Returns the predicate of one argument that checks if its argument matches the given clause."
  (with-gensyms (arg)
    `(lambda (,arg)
       (match ,arg
	 ,@(loop for c in optima-clauses
	      collect (list c t))))))

(defun hash (&rest k/v-pairs)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (k v) on k/v-pairs by #'cddr
       do (setf (gethash k hash) v))
    hash))
