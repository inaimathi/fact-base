;;;; fact-base.lisp
(in-package #:fact-base)

(defclass fact-base ()
  ((id :reader id :initarg :id)
   (fact-id :accessor fact-id :initform 0)
   (delta :accessor delta :initform nil)
   (current :accessor current :initform nil)
   (index :accessor index :initarg :index)
   (history :accessor history :initform nil)))

(defun new-id (&optional prefix) 
  (intern (symbol-name (gensym prefix)) :keyword))

(defun make-fact-base (&key (indices '(:a :b :c)) (id (new-id "FB-")))
  (make-instance 'fact-base :index (make-index indices) :id id))

;;;;;;;;;; Basics
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

(defmethod file-name ((state fact-base))
  (string-downcase (symbol-name (id state))))

(defmethod next-id! ((state fact-base))
  (let ((res (fact-id state)))
    (incf (fact-id state))
    res))

(defmethod lookup ((state fact-base) &key a b c)
  (let ((ixs (decide-index a b c)))
    (cond ((not ixs)
	   (warn "No indices provided, returning current fact base...%")
	   (current state))
	  ((indexed? (index state) (first ixs))
	   (gethash (rest ixs) (gethash (first ixs) (table (index state)))))
	  (t
	   (warn "No relevant index found, traversing...~%")
	   (loop for f in (current state)
	      when (and (or (not a) (equal a (first f)))
			(or (not b) (equal b (second f)))
			(or (not c) (equal c (third f))))
	      collect f)))))

(defmethod select ((fn function) (lst list))
  (loop for fact in lst when (funcall fn fact) collect fact))

(defmethod insert ((fact list) (state list)) 
  (cons fact state))

(defmethod delete ((fact list) (lst list))
  (remove fact lst :test #'equal))

(defmethod project ((history list) &key min-time max-time)
  (let ((range-fn (make-range-fn min-time max-time)))
    (loop with res = (list)
       for entry in history when (funcall range-fn entry)
       do (setf res
		(match entry
		  ((list _ :insert fact)
		   (insert fact res))
		  ((list _ :delete fact)
		   (delete fact res))))
       finally (return res))))

;;;;;;;;;; Fact-base specific
(defmethod select ((fn function) (state fact-base))
  (select fn (current state)))

(defmethod project! ((state fact-base))
  (setf (current state) (project (reverse (history state)))))

(defmethod multi-insert! ((state fact-base) (b/c-pairs list))
  (loop with id = (next-id! state)
     for (b c) in b/c-pairs do (insert! (list id b c) state)
     finally (return id)))

(defmethod insert! ((fact list) (state fact-base))
  (assert (and (cddr fact) (not (cdddr fact))) nil "INSERT! :: A fact is a list of length 3")
  (let ((time (local-time:now))
	(id (first fact)))
    (when (>= id (fact-id state)) (setf (fact-id state) (+ 1 id)))
    (let ((h (list time :insert fact)))
      (push h (history state))
      (push h (delta state)))
    (insert! fact (index state))
    (push fact (current state))
    nil))

(defmethod delete! ((fact list) (state fact-base))
  (setf (current state) (delete fact (current state)))
  (push (list (local-time:now) :delete fact) (history state))
  (delete! fact (index state))
  nil)

;;;;;;;;;; /(De)?Serialization/i
(defun list->timestamp (timestamp-list)
  (destructuring-bind (day sec nsec) timestamp-list
    (local-time:make-timestamp :day day :sec sec :nsec nsec)))

(defun timestamp->list (timestamp)
  (list (local-time:day-of timestamp) (local-time:sec-of timestamp) (local-time:nsec-of timestamp)))

(defmethod read-entry! ((s stream))
  (awhen (read s nil nil)
    (cons (list->timestamp (car it))
	  (cdr it))))

(defmethod write-entry! ((entry list) (s stream))
  (format s "~s~%" (cons (timestamp->list (car entry)) (cdr entry))))

(defmethod write-entries! ((entries list) (file pathname) if-exists)
  (with-open-file (s file :direction :output :if-exists if-exists :if-does-not-exist :create)
    (dolist (entry (reverse entries))
      (write-entry! entry s))))

(defmethod write-entries! ((entries list) (file string) if-exists)
  (write-entries! entries (pathname file) if-exists))

(defmethod write-delta! ((state fact-base) &key (file-name (file-name state)))
  (ensure-directories-exist file-name)
  (write-entries! (delta state) file-name :append)
  (setf (delta state) nil)
  file-name)

(defmethod write! ((state fact-base) &key (file-name (file-name state)))
  (ensure-directories-exist file-name)
  (write-entries! (history state) file-name :supersede)
  (setf (delta state) nil)
  file-name)

(defmethod read! ((s stream) &key min-time max-time)
  (let ((range-fn (make-range-fn min-time max-time)))
    (loop for entry = (read-entry! s) while entry for ts = (first entry)
       when (funcall range-fn entry) collect entry into es
       maximize (match entry
		  ((list _ :insert (list id _ _)) id)
		  (_ 0)) into max-id
       finally (return (values es max-id)))))

(defmethod read! ((file-name string) &key min-time max-time)
  (when (cl-fad:file-exists-p file-name)
    (with-open-file (s file-name :direction :input)
      (read! s :min-time min-time :max-time max-time))))

(defmethod index! ((state fact-base) (indices list))
  (setf (index state) (make-index indices))
  (map-insert! (current state) (index state))
  nil)

(defmethod load! ((file-name string) &key (indices '(:a :b :c)))
  (let ((res (make-fact-base 
	      :id (intern (string-upcase file-name) :keyword)
	      :indices indices)))
    (multiple-value-bind (es id) (read! file-name)
      (setf (history res) (reverse es)
	    (fact-id res) (+ id 1)))
    (project! res)
    (map-insert! (current res) (index res))
    res))
