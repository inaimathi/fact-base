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
   (earliest-entry :accessor earliest-entry :initform nil :initarg :earliest-entry)
   (latest-entry :accessor latest-entry :initform nil :initarg :latest-entry)))

(defun make-fact-base (&key (indices '(:a :b :c)) (file-name (temp-file-name)))
  (make-instance 'fact-base :index (make-index indices) :file-name file-name))

;;;;;;;;;; Basics
(defmethod next-id! ((state fact-base))
  (let ((res (fact-id state)))
    (incf (fact-id state))
    res))

(defmethod lookup ((state list) &key a b c)
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

(defmethod apply-entry ((state list) (entry list))
  (match entry
    ((list _ :insert fact)
     (insert state fact))
    ((list _ :change (list fact-a fact-b))
     (insert (delete state fact-a) fact-b))
    ((list _ :delete fact)
     (delete state fact))))

(defmethod reverse-entry ((state list) (entry list))
  (match entry
    ((list _ :insert fact)
     (delete state fact))
    ((list _ :change (list fact-a fact-b))
     (insert (delete state fact-b) fact-a))
    ((list _ :delete fact)
     (insert state fact))))

;;;;;;;;;; Fact-base specific
(defun update-id! (state fact)
  (when (>= (first fact) (fact-id state))
    (setf (fact-id state) (+ 1 (first fact)))))

(defmethod apply-entry! ((state fact-base) (entry list))
  (let ((ts (first entry)))
    (with-slots (earliest-entry latest-entry current) state
      (setf earliest-entry (local-time:timestamp-minimum (or earliest-entry ts) ts)
	    latest-entry (local-time:timestamp-maximum (or latest-entry ts) ts)))
    (match entry
      ((list _ :insert fact) 
       (insert-fact-internal! state fact :update-delta? nil))
      ((list _ :change (list old new))
       (change-fact-internal! state old new :update-delta? nil))
      ((list _ :delete fact)
       (delete-fact-internal! state fact :update-delta? nil)))))

(defmethod reverse-entry! ((state fact-base) (entry list))
  (match entry
    ((list _ :insert fact)
     (delete-fact-internal! state fact :update-delta? nil))
    ((list _ :change (list old new))
     (change-fact-internal! state new old :update-delta? nil))
    ((list _ :delete fact)
     (insert-fact-internal! state fact :update-delta? nil))))

(defmethod multi-insert! ((state fact-base) (b/c-pairs list))
  (loop with id = (next-id! state)
     for (b c) in b/c-pairs do (insert! state (list id b c))
     finally (return id)))

(defmethod insert-new! ((state fact-base) b c)
  (let ((id (next-id! state)))
    (insert! state (list id b c))
    id))

(defun insert-fact-internal! (state fact &key (update-delta? t))
  (assert (fact-p fact) nil "INSERT! :: A fact is a list of length 3: ~s" fact)
  (update-id! state fact)
  (when update-delta?
    (push! (list (local-time:now) :insert fact) (delta state)))
  (insert! (index state) fact)
  (push fact (current state))
  nil)

(defmethod insert! ((state fact-base) (fact list))
  (insert-fact-internal! state fact))

(defun change-fact-internal! (state old new &key (update-delta? t))
  (assert (fact-p old) nil "CHANGE! [old] :: A fact is a list of length 3: ~s" old)
  (assert (fact-p new) nil "CHANGE! [new] :: A fact is a list of length 3: ~s" new)
  (setf (current state) (insert (delete (current state) old) new))
  (when update-delta?
    (push! (list (local-time:now) :change (list old new)) (delta state)))
  (update-id! state new)
  (delete! (index state) old)
  (insert! (index state) new)
  nil)

(defmethod change! ((state fact-base) (old list) (new list))
  (change-fact-internal! state old new))

(defun delete-fact-internal! (state fact &key (update-delta? t))
  (assert (fact-p fact) nil "DELETE! :: A fact is a list of length 3: ~s" fact)
  (setf (current state) (delete (current state) fact))
  (when update-delta?
    (push! (list (local-time:now) :delete fact) (delta state)))
  (delete! (index state) fact)
  nil)

(defmethod delete! ((state fact-base) (fact list))
  (delete-fact-internal! state fact))

;;;;;;;;;; /(De)?Serialization/i
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

(defmethod write! ((state fact-base) &key (file-name (file-name state)) (zero-delta? t))
  (ensure-directories-exist file-name)
  (write-entries! (entries (delta state)) file-name :append)
  (when zero-delta? (setf (delta state) (queue)))
  file-name)

(defmethod index! ((state fact-base) (indices list))
  (setf (index state) (make-index indices))
  (map-insert! (current state) (index state))
  nil)

(defmethod load! ((file-name pathname) &key (indices '(:a :b :c)))
  (assert (cl-fad:file-exists-p file-name) nil "Nonexistent file ~s" file-name)
  (let ((res (make-fact-base :indices indices :file-name file-name)))
    (with-open-file (s file-name :direction :input)
      (loop for entry = (read-entry! s) while entry
	 do (apply-entry! res entry)))
    res))
