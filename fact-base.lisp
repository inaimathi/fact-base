;;;; fact-base.lisp

(in-package #:fact-base)

(defclass fact-base ()
  ((id :reader id :initarg :id)
   (current :accessor current :initform nil)
   (last-saved :accessor last-saved :initform 0)
   (history :accessor history :initform nil)))

;;;;;;;;;; Basics
(defmacro matching? (&rest optima-clauses)
  "Takes an optima match clause. 
Returns the predicate of one argument that checks if its argument matches the given clause."
  (with-gensyms (arg)
    `(lambda (,arg)
       (match ,arg
	 ,@(loop for c in optima-clauses
	      collect (list c t))))))

(defmethod file-name ((state fact-base))
  (string-downcase (symbol-name (id state))))

(defmethod select ((fn function) (lst list))
  (loop for fact in lst when (funcall fn fact) collect fact))

(defmethod insert ((fact list) (state list)) 
  (cons fact state))

(defmethod delete ((fn function) (lst list)) 
  (remove-if fn lst))

(defmethod calculate-current ((history list))
  (loop with res = (list)
     for (time type f) in history
     do (setf res
	      (case type
		(insert (insert f res))
		(delete (delete (eval `(matching? ,f)) res))))
     finally (return res)))

;;;;;;;;;; Fact-base specific
(defmethod select ((fn function) (state fact-base))
  (select fn (current state)))

(defmethod calculate-current! ((state fact-base))
  (setf (current state) (calculate-current! (reverse (history state)))))

(defmethod insert! ((fact list) (state fact-base))
  (assert (and (cddr fact) (not (cdddr fact))) nil "INSERT! :: A fact is a list of length 3")
  (let ((time (get-universal-time)))
    (push fact (current state))
    (push (list time 'insert fact) (history state))
    nil))

(defmacro delete! (match-clause state)
  (with-gensyms (s time fn)
    `(let* ((,s ,state)
	    (,time (get-universal-time))
	    (,fn (matching? ,match-clause)))
       (setf (current ,s) (delete ,fn (current ,s)))
       (push (list ,time 'delete ',match-clause) (history ,s))
       nil)))

;;;;;;;;;; /(De)?Serialization/i
(defmethod update! ((state fact-base) &key (file-name (file-name state)))
  (ensure-directories-exist file-name)
  (with-open-file (s file-name :direction :output :if-exists :append :if-does-not-exist :create)
    (loop with latest = (last-saved state)
       for rec in (reverse (history state)) for time = (first rec)
       when (> time latest) do (format s "~s~%" rec)
       finally (setf (last-saved state) (caar (history state))))))

(defmethod write! ((state fact-base) &key (file-name (file-name state)))
  (ensure-directories-exist file-name)
  (with-open-file (s file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for rec in (reverse (history state)) do (format s "~s~%" rec)
       finally (setf (last-saved state) (caar (history state))))))

(defmethod read! ((file-name string) &key (min-time 0) (max-time :now))
  (when (cl-fad:file-exists-p file-name)
    (with-open-file (s file-name :direction :input)
      (let ((range-fn 
	     (if (numberp max-time)
		 (lambda (time) (>= max-time time min-time))
		 (lambda (time) (>= time min-time)))))
	(reverse (loop for rec = (read s nil nil) while rec
		    for (time type f) = rec
		    when (funcall range-fn time) collect rec))))))
