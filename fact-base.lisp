;;;; fact-base.lisp
(in-package #:fact-base)

(defclass index ()
  ((table :reader table :initform (make-hash-table :test 'equal))))

(defclass fact-base ()
  ((file-name :reader file-name :initarg :file-name 
	      :documentation "The file associated with this fact-base")
   (fact-id :accessor fact-id :initform 0 
	    :documentation "The next free fact-id")
   (delta :accessor delta :initform (queue) 
	  :documentation "A collection of history entries that have not yet been written to disk")
   (current :accessor current :initform nil
	    :documentation "The current projection of this fact-base")
   (index :accessor index :initarg :index
	  :documentation "The index structure of the current projection (used to accelerate queries)")
   (entry-count :accessor entry-count :initform 0 
		:documentation "The count of entries in disk history.")
   (earliest-entry :accessor earliest-entry :initform nil :initarg :earliest-entry
		   :documentation "The earliest entry in the disk history.")
   (in-memory? :reader in-memory? :initarg :in-memory? :initform nil
	       :documentation "Flag that designates whether this fact-base is going to keep its history in memory or on disk.
For fact bases with a relatively short history, and/or ones with a requirement for high-frequency rewind, keeping it in memory makes more sense.
If you don't need rewinding very often or quickly, or will keep a very deep history for a particular notebook, maybe keep it on disk.")
   (history :reader history :initform nil :initarg :history
	    :documentation "If `in-memory?` is true, this should be a queue of this fact-bases' history entries.")
   (latest-entry :accessor latest-entry :initform nil :initarg :latest-entry
		 :documentation "The latest entry in the disk history.")))

(defun make-fact-base (&key (indices '(:a :b :c)) (file-name (temp-file-name)) in-memory?)
  (make-instance 'fact-base :index (make-index indices) :file-name file-name 
		 :in-memory? in-memory? :history (when in-memory? (queue))))

;;;;;;;;;; Basics
(defmethod total-entries ((state fact-base))
  (+ (entry-count state) (entry-count (delta state))))

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
     (delete state fact))
    (_ state)))

(defmethod reverse-entry ((state list) (entry list))
  (match entry
    ((list _ :insert fact)
     (delete state fact))
    ((list _ :change (list fact-a fact-b))
     (insert (delete state fact-b) fact-a))
    ((list _ :delete fact)
     (insert state fact))
    (_ state)))

(defun reverse-from-end-until (state fn)
  (let ((res (current state))
	(affected (make-hash-table)))
    (flet ((rev! (e)
	     (setf res (reverse-entry res e))
	     (when (listp (third e))
	       (setf (gethash (caaddr e) affected) t))))
      (unless (loop for e in (reverse (entries (delta state)))
		 if (funcall fn e) return t
		 else do (rev! e))
	(if (in-memory? state)
	    (loop for e in (reverse (entries (history state)))
	       until (funcall fn e) do (rev! e))
	    (with-open-elif (s (file-name state))
	      (loop for e = (read-entry-from-end! s) while e
		 until (funcall fn e) do (rev! e))))))
    (values res (alexandria:hash-table-keys affected))))

(defmethod rewind-to ((state fact-base) (time timestamp))
  (let ((latest (or (caar (last-cons (delta state))) (latest-entry state))))
    (cond ((local-time:timestamp>= time latest)
	   (values (current state) nil))
	  ((local-time:timestamp>= (earliest-entry state) time)
	   (values nil (mapcar #'car (current state))))
	  (t 
	   (reverse-from-end-until
	    state (lambda (e) (local-time:timestamp>= time (first e))))))))

(defmethod rewind-to ((state fact-base) (index integer))
  (let ((total (total-entries state)))
    (rewind-by-internal state (min (max (- total index) 0) total))))

(defmethod rewind-to ((state fact-base) (tag string))
  (reverse-from-end-until
   state (lambda (e) (and (eql (second e) :tag) (string= (third e) tag)))))

(defmethod rewind-by-internal ((state fact-base) (count integer))
  (cond ((zerop count)
	 (values (current state)))
	((>= count (+ (entry-count state) (entry-count (delta state))))
	 (values nil (mapcar #'car (current state))))
	(t (reverse-from-end-until
	    state (let ((ct count))
		    (lambda (e) 
		      (declare (ignore e))
		      (format t "Reversing one...~%")
		      (if (zerop ct)
			  t
			  (progn (decf ct) nil))))))))

(defmethod rewind-by ((state fact-base) delta &optional (units :entries))
  (assert (member units '(:entries :months :years :nanoseconds :seconds :minutes :hours :days))
	  nil "units must be one of ~s" 
	  '(:entries :months :years :nanoseconds :seconds :minutes :hours :days))
  (if (eql units :entries)
      (rewind-by-internal state delta)
      (let ((latest (or (caar (last-cons (delta state))) (latest-entry state))))
	(rewind-to state (local-time:timestamp- 
			      latest delta 
			      (case units
				(:months :month)
				(:years :year)
				(:nanoseconds :nsec)
				(:seconds :sec)
				(:minutes :minute)
				(:hours :hour)
				(:days :day)))))))

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

(defmethod tag! ((state fact-base) (tag string))
  (push! (list (local-time:now) :tag tag) (delta state))
  nil)

(defun insert-fact-internal! (state fact &key (update-delta? t))
  (assert (fact-p fact) nil "INSERT! :: A fact is a list of length 3: ~s" fact)
  (update-id! state fact)
  (let ((entry (list (local-time:now) :insert fact)))
    (when update-delta? (push! entry (delta state)))
    (when (in-memory? state) (push! entry (history state))))
  (insert! (index state) fact)
  (push fact (current state))
  nil)

(defmethod insert! ((state fact-base) (fact list))
  (insert-fact-internal! state fact))

(defun change-fact-internal! (state old new &key (update-delta? t))
  (assert (fact-p old) nil "CHANGE! [old] :: A fact is a list of length 3: ~s" old)
  (assert (fact-p new) nil "CHANGE! [new] :: A fact is a list of length 3: ~s" new)
  (setf (current state) (insert (delete (current state) old) new))
  (let ((entry (list (local-time:now) :change (list old new))))
    (when update-delta? (push! entry (delta state)))
    (when (in-memory? state) (push! entry (history state))))
  (update-id! state new)
  (delete! (index state) old)
  (insert! (index state) new)
  nil)

(defmethod change! ((state fact-base) (old list) (new list))
  (change-fact-internal! state old new))

(defun delete-fact-internal! (state fact &key (update-delta? t))
  (assert (fact-p fact) nil "DELETE! :: A fact is a list of length 3: ~s" fact)
  (setf (current state) (delete (current state) fact))
  (let ((entry (list (local-time:now) :delete fact)))
    (when update-delta? (push! entry (delta state)))
    (when (in-memory? state) (push! entry (history state))))
  (delete! (index state) fact)
  nil)

(defmethod delete! ((state fact-base) (fact list))
  (delete-fact-internal! state fact))

;;;;;;;;;; /(De)?Serialization/i
(defmethod read-entry! ((s stream))
  (awhen (read s nil nil)
    (cons (list->timestamp (car it))
	  (cdr it))))

(defmethod read-entry-from-end! ((s stream) &key (skip 0))
  "Only use this inside of `with-open-elif`.
It's just an EXTREMELY expensive, non forwarding version of read-entry! otherwise.
Takes a stream opened with `with-open-elif`, returns a history entry from the end of that file.
Two keyword arguments:
  - :skip  - is a number of entries to skip before the one we want (defaults to 0, which gives the last one)"
  (assert (>= skip 0) nil "I can't skip a negative number, Dave.")
  (let ((cur (file-position s))
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
      (loop repeat (+ skip 1) until (zerop cur)
	 do (to-entry-start))
      (let ((fp (file-position s))
	    (res (read-entry! s)))
	(file-position s fp)
	res))))

(defmethod write-entry! ((entry list) (s stream))
  (format s "~s~%" (cons (timestamp->list (car entry)) (cdr entry))))

(defmethod write-entries! ((entries list) (file pathname) if-exists)
  (with-open-file (s file :direction :output :if-exists if-exists :if-does-not-exist :create)
    (dolist (entry entries)
      (write-entry! entry s))))

(defmethod write-entries! ((entries list) (file string) if-exists)
  (write-entries! entries (pathname file) if-exists))

(defmethod write! ((state fact-base) &key (file-name (file-name state)))
  (ensure-directories-exist file-name)
  (write-entries! (entries (delta state)) file-name :append)
  (with-slots (delta entry-count earliest-entry latest-entry) state
    (setf entry-count (+ entry-count (entry-count delta))
	  earliest-entry (or earliest-entry (caar (entries delta)))
	  latest-entry (caar (last-cons delta))
	  delta (queue)))
  file-name)

(defmethod index! ((state fact-base) (indices list))
  (setf (index state) (make-index indices))
  (map-insert! (current state) (index state))
  nil)

(defmethod load! ((file-name pathname) &key (indices '(:a :b :c)) in-memory?)
  (assert (cl-fad:file-exists-p file-name) nil "Nonexistent file ~s" file-name)
  (let ((res (make-fact-base :indices indices :file-name file-name :in-memory? in-memory?)))
    (with-open-file (s file-name :direction :input)
      (loop for entry = (read-entry! s) while entry
	 do (incf (entry-count res))
	 when in-memory? do (push! entry (history res))
	 do (apply-entry! res entry)))
    res))
