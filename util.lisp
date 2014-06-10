(in-package :fact-base)

(defmethod fact-p (fact) nil)
(defmethod fact-p ((fact list)) (and (cddr fact) (not (cdddr fact))))

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
