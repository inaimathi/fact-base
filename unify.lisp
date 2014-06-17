(in-package :fact-base)

;;; Some contents ripped bleeding from PAIP.
;;; http://norvig.com/paip/README.html

(defvar +fail+ (gensym "FAIL"))

(defun fail? (thing) (eq thing +fail+))
(defun variable? (thing)
  (and (symbolp thing) (eql #\? (char (symbol-name thing) 0))))

(defun variables-in (thing)
  (unique-find-anywhere-if #'variable? thing))

(defmethod pre-search (goal-form facts)
  (destructuring-bind (a b c) goal-form
    (lookup facts
	    :a (unless (any-variables? a) a)
	    :b (unless (any-variables? b) b)
	    :c (unless (any-variables? c) c))))

(defun occurs-check (var x bindings)
  (cond ((eq var x) t)
	((and (variable? x) (get-binding x bindings))
	 (occurs-check var (lookup-binding x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))

(defun unify-variable (var x bindings)
  (cond ((get-binding var bindings)
	 (unify (lookup-binding var bindings) x bindings))
	((and (variable? x) (get-binding x bindings))
	 (unify var (lookup-binding x bindings) bindings))
	((occurs-check var x bindings)
	 +fail+)
	(t (extend-bindings var x bindings))))

(defun unify (x y &optional bindings)
  (cond ((fail? bindings) +fail+)
	((eql x y) bindings)
	((variable? x) (unify-variable x y bindings))
	((variable? y) (unify-variable y x bindings))
	((and (stringp x) (stringp y))
	 (if (string= x y) bindings +fail+))
	((and (consp x) (consp y))
	 (unify (rest x) (rest y)
		(unify (first x) (first y) bindings)))
	(t +fail+)))

(defun single-goal (base goal-form &optional bindings)
  (let ((fs (pre-search goal-form base)))
    (lambda ()
      (loop while fs for f = (pop fs) for res = (unify goal-form f bindings)
	 unless (fail? res) return res
	 finally (return +fail+)))))

(defun lisp-goals (goals &optional bindings)
  (let ((g `(and ,@(subst-bindings bindings goals)))
	(called? nil))
    (lambda ()
      (if (and (not called?) (eval g))
	  (progn (setf called? t)
		 bindings)
	  +fail+))))

(defun not-goals (base goals &optional bindings)
  (let ((gens (loop for g in goals collect (make-goal base g bindings)))
	(called? nil))
    (lambda ()
      (if called?
	  +fail+
	  (loop for g in gens for res = (funcall g)
	     unless (fail? res) return +fail+
	     finally (progn 
		       (setf called? t)
		       (return bindings)))))))

(defun or-goals (base goals &optional bindings)
  (let* ((gens (loop for g in goals collect (make-goal base g bindings)))
	 (gs gens))
    (labels ((next ()
	       (cond ((and gens gs)
		      (let* ((g (pop gs))
			     (res (funcall g)))
			(if (fail? res)
			    (progn (setf gens (remove g gens))
				   (next))
			    res))) 
		     (gens
		      (setf gs gens)
		      (next))
		     (t 
		      +fail+))))
      #'next)))

(defun and-goals (base goals &optional bindings)
  (let ((gen (make-goal base (first goals) bindings)))
    (if (null (rest goals))
	gen
	(let ((rest-gen))
	  (labels ((next-gen ()
		     (let ((res (funcall gen)))
		       (if (fail? res)
			   +fail+
			   (setf rest-gen (and-goals base (rest goals) res)))))
		   (backtrack! ()
		     (if (fail? (next-gen))
			 +fail+
			 (next)))
		   (next ()
		     (if (null rest-gen)
			 (backtrack!)
			 (let ((res (funcall rest-gen)))
			   (if (fail? res)
			       (backtrack!)
			       res)))))
	    #'next)))))

(defun make-goal (base goal &optional bindings)
  (if (symbolp (first goal))
      (case (->key (first goal))
	(:and (and-goals base (rest goal) bindings))
	(:or (or-goals base (rest goal) bindings))
	(:not (not-goals base (rest goal) bindings))
	(:lisp (lisp-goals (rest goal) bindings))
	(t (single-goal base goal bindings)))
      (single-goal base goal bindings)))

(defmacro for-all (goal-term &key in collect do)
  (with-gensyms (gen res)
    `(let ((,gen ,(if (and (symbolp (first goal-term))
			   (member (->key (first goal-term))
				   (list :quote :backq-list :backq-list*)))
		      `(make-goal ,in ,goal-term)
		      `(make-goal ,in ',goal-term))))
       (loop for ,res = (funcall ,gen)
	  until (fail? ,res)
	    ,(if do 'do 'collect)
	    ,(if (or collect do)
		 `(apply (lambda ,(variables-in goal-term)
			   ,(or collect do))
			 (subst-bindings ,res ',(variables-in goal-term)))
		 `(subst-bindings ,res ',(variables-in goal-term)))))))
