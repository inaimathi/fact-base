;;;; fact-base.lisp
(in-package #:fact-base)

(defclass index ()
  ((table :reader table :initform (make-hash-table :test 'equal))))

(defclass fact-base ()
  ((file-name :reader file-name :initarg :file-name)
   (fact-id :accessor fact-id :initform 0)
   (delta :accessor delta :initform (queue))
   (current :accessor current :initform nil)
   (index :accessor index :initarg :index)
   (history :accessor history :initform (queue))))

(defun make-fact-base (&key (indices '(:a :b :c)) (file-name (temp-file-name)))
  (make-instance 'fact-base :index (make-index indices) :file-name file-name))

;;;;;;;;;; Basics
(defmethod next-id! ((state fact-base))
  (let ((res (fact-id state)))
    (incf (fact-id state))
    res))

(defmethod lookup ((state list) &key a b c)
  (warn "Given base is a list; traversing...")
  (if (and (not a) (not b) (not c))
      state
      (loop for f in state
	 when (and (or (not a) (equal a (first f)))
		   (or (not b) (equal b (second f)))
		   (or (not c) (equal c (third f))))
	 collect f)))

(defmethod lookup ((state fact-base) &key a b c)
  (if (every #'not (list a b c))
      (current state)
      (multiple-value-bind (index ideal-index) (decide-index state a b c)
	(let ((ix (if index
		      (gethash (rest index) (gethash (first index) (table (index state))))
		      (current state))))
	  (if (and index (eq (first index) ideal-index))
	      ix
	      (loop for f in ix
		 when (and (or (not a) (equal a (first f)))
			   (or (not b) (equal b (second f)))
			   (or (not c) (equal c (third f))))
		 collect f))))))

(defmethod insert ((state list) (fact list)) 
  (cons fact state))

(defmethod delete ((state list) (fact list))
  (remove fact state :test #'equal))

(defmethod history-slice ((history list) &key min-time max-time)
  (let ((range-fn (make-range-fn min-time max-time)))
    (loop for entry in history 
       when (funcall range-fn entry) collect entry)))

(defmethod history-slice ((state fact-base) &key min-time max-time)
  (history-slice (history state) :min-time min-time :max-time max-time))

(defmethod project ((history list) &key min-time max-time)
  (let ((range-fn (make-range-fn min-time max-time)))
    (loop with res = (list)
       for entry in history when (funcall range-fn entry)
       do (setf res
		(match entry
		  ((list _ :insert fact)
		   (insert res fact))
		  ((list _ :change (list fact-a fact-b))
		   (insert (delete res fact-a) fact-b))
		  ((list _ :delete fact)
		   (delete res fact))))
       finally (return res))))

;;;;;;;;;; Fact-base specific
(defmethod project! ((state fact-base))
  (setf (current state) (project (entries (history state)))))

(defmethod multi-insert! ((state fact-base) (b/c-pairs list))
  (loop with id = (next-id! state)
     for (b c) in b/c-pairs do (insert! state (list id b c))
     finally (return id)))

(defmethod insert-new! ((state fact-base) b c)
  (let ((id (next-id! state)))
    (insert! state (list id b c))
    id))

(defmethod insert! ((state fact-base) (fact list))
  (assert (fact-p fact) nil "INSERT! :: A fact is a list of length 3: ~s" fact)
  (let ((time (local-time:now))
	(id (first fact)))
    (when (>= id (fact-id state)) (setf (fact-id state) (+ 1 id)))
    (let ((h (list time :insert fact)))
      (push! h (history state))
      (push! h (delta state)))
    (insert! (index state) fact)
    (push fact (current state))
    nil))

(defmethod change! ((state fact-base) (old list) (new list))
  (assert (fact-p old) nil "CHANGE! [old] :: A fact is a list of length 3: ~s" old)
  (assert (fact-p new) nil "CHANGE! [new] :: A fact is a list of length 3: ~s" new)
  (setf (current state) (insert (delete (current state) old) new))
  (let ((h (list (local-time:now) :change (list old new))))
    (push! h (history state))
    (push! h (delta state)))
  (delete! (index state) old)
  (insert! (index state) new)
  nil)

(defmethod delete! ((state fact-base) (fact list))
  (assert (fact-p fact) nil "DELETE! :: A fact is a list of length 3: ~s" fact)
  (setf (current state) (delete (current state) fact))
  (let ((h (list (local-time:now) :delete fact)))
    (push! h (history state))
    (push! h (delta state)))
  (delete! (index state) fact)
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
    (dolist (entry entries)
      (write-entry! entry s))))

(defmethod write-entries! ((entries list) (file string) if-exists)
  (write-entries! entries (pathname file) if-exists))

(defmethod write-delta! ((state fact-base) &key (file-name (file-name state)) (zero-delta? t))
  (ensure-directories-exist file-name)
  (write-entries! (entries (delta state)) file-name :append)
  (when zero-delta? (setf (delta state) (queue)))
  file-name)

(defmethod write! ((state fact-base) &key (file-name (file-name state)) (zero-delta? t))
  (ensure-directories-exist file-name)
  (write-entries! (entries (history state)) file-name :supersede)
  (when zero-delta? (setf (delta state) (queue)))
  file-name)

(defmethod read! ((s stream) &key min-time max-time)
  (let ((range-fn (make-range-fn min-time max-time)))
    (loop for entry = (read-entry! s) while entry for ts = (first entry)
       when (funcall range-fn entry) collect entry into es
       maximize (match entry
		  ((list _ :insert (list id _ _)) id)
		  ((list _ :change (list id _ _)) id)
		  (_ 0)) into max-id
       finally (return (values es max-id)))))

(defmethod read! ((file-name pathname) &key min-time max-time)
  (when (cl-fad:file-exists-p file-name)
    (with-open-file (s file-name :direction :input)
      (read! s :min-time min-time :max-time max-time))))

(defmethod index! ((state fact-base) (indices list))
  (setf (index state) (make-index indices))
  (map-insert! (current state) (index state))
  nil)

(defmethod load! ((file-name pathname) &key (indices '(:a :b :c)))
  (let ((res (make-fact-base :indices indices :file-name file-name)))
    (multiple-value-bind (es id) (read! file-name)
      (setf (history res) (queue es)
	    (fact-id res) (+ id 1)))
    (project! res)
    (map-insert! (index res) (current res))
    res))
