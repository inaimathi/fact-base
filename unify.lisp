(in-package :fact-base)

;;; Mostly ripped bleeding from PAIP. It's certainly worth a read.

;;;;; Basics
(defvar +fail+ nil)
(defvar +succeed+ '((t . t)))

(defun fail? (thing) (eq thing +fail+))
(defun fail () +fail+)

;;;;; Bindings/variables
(defun variable? (thing)
  (and (symbolp thing) (eql #\? (char (symbol-name thing) 0))))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun get-bind-val (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
	(if (eq bindings +succeed+)
	    nil
	    bindings)))

(defun match-variable (var input bindings)
  (let ((b (get-binding var bindings)))
    (cond ((not b) (extend-bindings var input bindings))
	  ((equal input (binding-val b)) bindings)
	  (t (fail)))))

;;;;; Unification
(defun occurs-check (var x bindings)
  (cond ((eq var x) t)
	((and (variable? x) (get-binding x bindings))
	 (occurs-check var (get-bind-val x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))

(defun unify-variable (var x bindings &optional occurs-check?)
  (cond ((get-binding var bindings)
	 (unify (get-bind-val var bindings) x bindings))
	((and (variable? x) (get-binding x bindings))
	 (unify var (get-bind-val x bindings) bindings))
	((and occurs-check? (occurs-check var x bindings))
	 (fail))
	(t (extend-bindings var x bindings))))

(defun unify (x y &optional (bindings +succeed+))
  (cond ((fail? bindings) (fail))
	((eql x y) bindings)
	((and (stringp x) (stringp y) (string= x y))
	 bindings)
	((variable? x) (unify-variable x y bindings))
	((variable? y) (unify-variable y x bindings))
	((and (consp x) (consp y))
	 (unify (rest x) (rest y)
		(unify (first x) (first y) bindings)))
	(t (fail))))

(defun subst-bindings (bindings x)
  (cond ((fail? bindings) (fail))
	((eq bindings +succeed+) x)
	((and (variable? x) (get-binding x bindings))
	 (subst-bindings bindings (get-bind-val x bindings)))
	((atom x) x)
	(t (cons (subst-bindings bindings (first x))
		 (subst-bindings bindings (rest x))))))

(defun unifier (x y)
  (subst-bindings (unify x y) x))

;;;;; Traverses
(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if
       predicate (first tree)
       (unique-find-anywhere-if predicate (rest tree) found-so-far))))

(defun replace-anonymous (exp)
  (cond ((eq exp '?) (gensym "?"))
	((atom exp) exp)
	(t (cons (replace-anonymous (first exp))
		 (replace-anonymous (rest exp))))))

(defun variables-in (exp)
  (unique-find-anywhere-if 
   (lambda (v) (and (variable? v) (not (eq '? v))))
   exp))

(defun any-variables? (exp)
  (if (atom exp)
      (variable? exp)
      (loop for e in exp 
	 if (listp e) do (any-variables? e)
	 else if (variable? e) do (return t)
	 finally (return nil))))

(defmacro no-vars-in? (&rest terms)
  `(and ,@(loop for tr in terms
	     collect `(not (any-variables? ,tr)))))

(defmethod use-index? (goal (state index))
  (destructuring-bind (a b c) goal
    (cond ((and (indexed? state :abc) (no-vars-in? a b c))
	   (list :abc a b c)) 
	  ((and (indexed? state :ab) (no-vars-in? a b))
	   (list :ab a b))
	  ((and (indexed? state :ac) (no-vars-in? a c)) 
	   (list :ac a c))
	  ((and (indexed? state :bc) (no-vars-in? b c))
	   (list :bc b c))
	  ((and (indexed? state :a) (no-vars-in? a))
	   (list :a a))
	  ((and (indexed? state :b) (no-vars-in? b))
	   (list :b b))
	  ((and (indexed? state :c) (no-vars-in? c))
	   (list :c c)))))

(defmethod match-single (goal bindings (facts fact-base))
  (match-single 
   goal bindings 
   (let ((ix (use-index? goal (index facts))))
     (if ix
	 (gethash (rest ix) (gethash (first ix) (table (index facts))))
	 (current facts)))))

(defmethod match-single (goal bindings (facts list))
  (let ((fs facts))
    (lambda ()
      (loop for res = (unify goal (pop fs) bindings)
	 unless (fail? res) do (return res)
	 while fs
	 finally (return (fail))))))

(defmethod match-ors (goals bindings (facts fact-base))
  (let ((fs facts))
    (flet ((try-goals (f)
	     (loop for g in goals 
		for res = (unify g f bindings) when res do (return res)
		finally (return (fail)))))
      (lambda ()
	(loop for res = (try-goals (pop fs))
	   unless (fail? res) do (return res)
	   while fs
	   finally (return (fail)))))))

(defmethod match-ands (goals bindings (facts fact-base))
  (let ((generator (match-single (first goals) bindings facts))
	(rest-generator))
    (if (null (cdr goals))
	generator
	(labels ((next-gen ()
		   (let ((res (funcall generator)))
		     (if (fail? res)
			 (fail)
			 (setf rest-generator (match-ands (rest goals) res facts)))))
		 (backtrack! ()
		   (if (fail? (next-gen))
		       (fail)
		       (next)))
		 (next ()
		   (if (null rest-generator)
		       (backtrack!)
		       (let ((res (funcall rest-generator)))
			 (if (fail? res)
			     (backtrack!)
			     res)))))
	  #'next))))

(defmacro for-all (goal-term &key in get apply)
  (assert in nil "Need a database to query...")
  (when (and get apply)
    (format t ":apply and :get arguments passed in; ignoring :get"))
  (with-gensyms (template gen res facts)
    `(let* ((,facts ,in)
	    (,gen ,(cond ((eq 'and (car goal-term))
			  `(match-ands ',(replace-anonymous (rest goal-term)) +succeed+ ,facts))
			 ((eq 'or (car goal-term))
			  `(match-ors ',(replace-anonymous (rest goal-term)) +succeed+ ,facts))
			 (t
			  `(match-single ',goal-term +succeed+ ,facts))))
	    ,@(unless apply
	      `((,template ',(replace-anonymous (or get goal-term))))))
       (loop for ,res = (funcall ,gen)
	  while ,res collect ,(if apply
				  `(apply (lambda ,(variables-in apply) ,apply)
					  (subst-bindings ,res ',(variables-in apply)))
				  `(subst-bindings ,res ,template))))))

;;;;; Test data
(defparameter *base* (make-fact-base))
(insert! *base* (list 0 :message "This is a sample message"))
(insert! *base* (list 1 :message "This is another one"))
(insert! *base* (list 1 :author "Inaimathi"))
(insert! *base* (list 2 :message "That second one was written by me. This one is a meta-message (also by me)."))
(insert! *base* (list 2 :author "Inaimathi"))
(insert! *base* (list 2 :type :meta))
