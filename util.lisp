(in-package :fact-base)

(defmethod fact-p (fact) nil)
(defmethod fact-p ((fact list)) (and (cddr fact) (not (cdddr fact))))

(defmethod ->key ((thing null)) thing)
(defmethod ->key ((thing symbol)) (intern (symbol-name thing) :keyword))

(defun temp-file-name ()
  (let* ((f (cl-fad:open-temporary :template "TEMPORARY-FILES:BASE-%"))
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

(defun hash (&rest k/v-pairs)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (k v) on k/v-pairs by #'cddr
       do (setf (gethash k hash) v))
    hash))

(defmethod key->symbols ((keyword symbol))
  (mapcar (lambda (c) (intern (coerce (list c) 'string)))
	  (coerce (symbol-name keyword) 'list)))

(defun list->timestamp (timestamp-list)
  (destructuring-bind (day sec nsec) timestamp-list
    (local-time:make-timestamp :day day :sec sec :nsec nsec)))

(defun timestamp->list (timestamp)
  (list (local-time:day-of timestamp) (local-time:sec-of timestamp) (local-time:nsec-of timestamp)))

;;;;;;;;;; Unification utils
(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if
       predicate (first tree)
       (unique-find-anywhere-if predicate (rest tree) found-so-far))))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-value (binding)
  (cdr binding))

(defun lookup-binding (var bindings)
  (binding-value (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val) bindings))

(defun subst-bindings (bindings term)
  (cond ((eq bindings +fail+) +fail+)
	((not bindings) term)
	((variable? term) 
	 (aif (get-binding term bindings)
	      (subst-bindings bindings (binding-value it))
	      term))
	((atom term) term)
	(t (cons (subst-bindings bindings (first term))
		 (subst-bindings bindings (rest term))))))

(defun any-variables? (exp)
  (if (atom exp)
      (variable? exp)
      (loop for e in exp 
	 if (listp e) do (any-variables? e)
	 else if (variable? e) do (return t)
	 finally (return nil))))

;;;;;;;;;; File-system utils
;; do we want an option for reading multiple entries? What would that need to look like?
(defun read-entry-from-end (fname &key (skip 0) start)
  "Takes a filename, returns a history entry from the end of that file.
Two keyword arguments:
  - :skip  - is a number of entries to skip before the one we want (defaults to 0, which gives the last one)
  - :start - is the position in the file to start searching. 
             It defaults to the end of the file.
             Careful here; if you pass it a position in the middle of an s-expression, things will explode."
  (assert (>= skip 0) nil "I can't skip a negative number, Dave.")
  (assert (or (null start) (>= start 0)) nil "I can't read negative bytes, Dave.")
  (with-open-file (s fname)
    (let* ((len (file-length s))
	   (cur (if start (min (- len 1) start) (- len 1)))
	   (paren-depth 0))
      (labels ((peek () (peek-char nil s))
	       (dec () (file-position s (decf cur)))
	       (to-quote ()
		 (loop for c = (peek) do (dec)
		    until (char= #\" c))
		 (slashes))
	       (slashes ()
		 (let ((ct 0))
		   (loop for c = (peek) while (char= #\\ c)
		      do (incf ct) do (dec))
		   (when (oddp ct) (to-quote))))
	       (to-entry-start ()
		 (loop for c = (peek)
		    do (dec)
		    do (case c
			 (#\" (to-quote))
			 (#\( (decf paren-depth))
			 (#\) (incf paren-depth)))
		    until (or (zerop cur) (and (char= #\( c) (zerop paren-depth))))))
	(file-position s cur)
	(loop repeat (+ skip 1) until (zerop cur)
	   do (to-entry-start))
	(let ((fp (file-position s)))
	  (values (read s) fp))))))
