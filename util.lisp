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
  (acons var val bindings))

(defun subst-bindings (bindings term)
  (cond ((eq bindings +fail+) +fail+)
	((not bindings) term)
	((variable? term)
	 (if-let (binding (get-binding term bindings))
           (subst-bindings bindings (binding-value binding))
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
(defmacro with-open-elif ((stream file-name) &body body)
  (with-gensyms (f)
    `(let ((,f ,file-name))
       (with-open-file (,stream ,f)
	 (file-position ,stream (- (file-length ,stream) 1))
	 ,@body))))
