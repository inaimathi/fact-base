(in-package :fact-base)

(defclass queue ()
  ((entries :accessor entries :initform nil :initarg :entries)
   (entry-count :accessor entry-count :initform 0 :initarg :entry-count)
   (last-cons :accessor last-cons :initform nil :initarg :last-cons)))

(defun queue (&optional elems) 
  (make-instance 
   'queue :entries elems :entry-count (length elems)
   :last-cons (when elems (last elems))))

(defmethod push! (entry (q queue))
  (let ((entry (list entry)))
    (if (empty? q)
	(setf (entries q) entry
	      (last-cons q) (entries q))
	(setf (cdr (last-cons q)) entry
	      (last-cons q) entry))
    (incf (entry-count q)))
  (entries q))

(defmethod pop! ((q queue))
  (decf (entry-count q))
  (pop (entries q)))

(defmethod empty? ((q queue)) (null (entries q)))
