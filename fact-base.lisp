;;;; fact-base.lisp
(in-package #:fact-base)

(defclass fact-base ()
  ((id :reader id :initarg :id)
   (fid :accessor fid :initform 0)
   (last-saved :accessor last-saved :initform (local-time:now))
   (current :accessor current :initform nil)
   (index :reader index :initform (make-index :a) :initarg :index)
   (history :accessor history :initform nil)))

(defun make-fact-base (indices)
  (make-instance 'fact-base :index (make-index indices)))

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
  (let ((res (fid state)))
    (incf (fid state))
    res))

(defmethod lookup ((state fact-base) &key a b c)
  (let ((ix (decide-index a b c)))
    (cond ((not ix)
	   (warn "No indices provided, returning current fact base...%")
	   (current state))
	  ((indexed? (index state) ix)
	   (index-by ix (index state) a b c))
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

(defmethod delete ((fn function) (lst list)) 
  (remove-if fn lst))

(defmethod project ((history list) &key min-time max-time)
  (let ((range-fn (make-range-fn min-time max-time)))
    (loop with res = (list)
       for entry in history when (funcall range-fn entry)
       do (setf res
		(match entry
		  ((list _ :insert fact)
		   (insert fact res))
		  ((list _ :delete fn _)
		   (delete fn res))))
       finally (return res))))

;;;;;;;;;; Fact-base specific
(defmethod select ((fn function) (state fact-base))
  (select fn (current state)))

(defmethod project! ((state fact-base))
  (setf (current state) (project (reverse (history state)))))

(defmethod multi-insert! ((b/c-pairs list) (state fact-base))
  (loop with id = (next-id! state)
     for (b c) in b/c-pairs do (insert! (list id b c) state)))

(defmethod insert! ((fact list) (state fact-base))
  (assert (and (cddr fact) (not (cdddr fact))) nil "INSERT! :: A fact is a list of length 3")
  (let ((time (local-time:now))
	(id (first fact)))
    (when (>= id (fid state)) (setf (fid state) (+ 1 id)))
    (insert! fact (index state))
    (push fact (current state))
    (push (list time :insert fact) (history state))
    nil))

;; (defmacro delete! (match-clause state)
;;   (with-gensyms (s time fn)
;;     `(let* ((,s ,state)
;; 	    (,time (local-time:now))
;; 	    (,fn (matching? ,match-clause)))
;;        (setf (current ,s) (delete ,fn (current ,s)))
;;        (push (list ,time :delete ,fn ',match-clause) (history ,s))
;;        nil)))

;;;;;;;;;; /(De)?Serialization/i
(defvar +epoch+ (local-time:universal-to-timestamp 0))

(defun list->timestamp (timestamp-list)
  (destructuring-bind (day sec nsec) timestamp-list
    (local-time:make-timestamp :day day :sec sec :nsec nsec)))

(defun timestamp->list (timestamp)
  (list (local-time:day-of timestamp) (local-time:sec-of timestamp) (local-time:nsec-of timestamp)))

(defmethod write-entry! ((entry list) (s stream))
  (match entry
    ((list ts :insert fact) 
     (format s "~s~%" (list (timestamp->list ts) :insert fact)))
    ((list ts :delete _ template)
     (format s "~s~%" (list (timestamp->list ts) :delete template)))))

(defmethod read-entry! ((s stream))
  (awhen (read s nil nil)
    (cons (list->timestamp (car it))
	  (match (cdr it)
	    ((list :delete template)
	     (list :delete (eval `(matching? ,template)) template))
	    (val val)))))

(defmethod update! ((state fact-base) &key (file-name (file-name state)))
  (ensure-directories-exist file-name)
  (with-open-file (s file-name :direction :output :if-exists :append :if-does-not-exist :create)
    (loop with latest = (last-saved state)
       for rec in (reverse (history state)) for time = (first rec)
       when (local-time:timestamp> time latest) do (write-entry! rec s)
       finally (setf (last-saved state) (caar (history state))))))

(defmethod write! ((state fact-base) &key (file-name (file-name state)))
  (ensure-directories-exist file-name)
  (with-open-file (s file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for rec in (reverse (history state)) do (write-entry! rec s)
       finally (setf (last-saved state) (caar (history state))))))

(defmethod read! ((file-name string) &key min-time max-time)
  (when (cl-fad:file-exists-p file-name)
    (with-open-file (s file-name :direction :input)
      (let ((range-fn (make-range-fn min-time max-time)))
	(loop with max-time = +epoch+
	   for entry = (read-entry! s) while entry for ts = (first entry)
	   when (funcall range-fn entry) collect entry into es
	   when (local-time:timestamp>= ts max-time) do (setf max-time ts)
	   maximize (match entry
		      ((list _ :insert (list id _ _)) id)
		      (_ 0)) into max-id
	   finally (return (values es max-time max-id)))))))

(defmethod load! ((file-name string))
  (let ((res (make-instance 'fact-base :id (intern (string-upcase file-name) :keyword))))
    (multiple-value-bind (es time id) (read! file-name)
      (setf (history res) (reverse es)
	    (fid res) (+ id 1)
	    (last-saved res) time))
    (calculate-current! res)
    res))
