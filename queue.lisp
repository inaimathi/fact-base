(in-package :fact-base)

(defclass queue ()
  ((entries :accessor entries :initform nil :initarg :entries)
   (last-cons :accessor last-cons :initform nil :initarg :last-cons)))

(defun queue (&optional elems) 
  (make-instance 'queue :entries elems :last-cons (when elems (last elems))))

(defmethod push! (entry (q queue))
  (let ((entry (list entry)))
    (if (empty? q)
	(setf (entries q) entry
	      (last-cons q) (entries q))
	(setf (cdr (last-cons q)) entry
	      (last-cons q) entry)))
  (entries q))

(defmethod pop! ((q queue))
  (pop (entries q)))

(defmethod empty? ((q queue)) (null (entries q)))
