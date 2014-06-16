(in-package :fact-base)

;;; Some contents ripped bleeding from PAIP.
;;; http://norvig.com/paip/README.html
;;; Minor alterations are present.

(defun variable? (thing)
  (and (symbolp thing) (eql #\? (char (symbol-name thing) 0))))

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

(defun literal? (thing)
  (or (null thing)
      (stringp thing) 
      (numberp thing)
      (keywordp thing)))

(defun goal->optima-clause (goal &key (bindings (make-hash-table)))
  (let ((guards nil))
    (labels ((make-name (elem)
	       (let ((name (gensym)))
		 (push (cons name elem) guards)
		 name))
	     (rec (elem)
	       (cond ((literal? elem)
		      elem)
		     ((and (listp elem) (not (listp (cdr elem))))
		      `(cons ,(rec (car elem)) ,(rec (cdr elem))))
		     ((listp elem)
		      (cons 'list (mapcar #'rec elem)))
		     ((eq '? elem)
		      '_)
		     ((not (variable? elem))
		      (make-name elem))
		     ((and (variable? elem) (gethash elem bindings))
		      (make-name elem))
		     ((variable? elem)
		      (setf (gethash elem bindings) t)
		      elem)
		     (t (error "Somethings' up. goal->optima-clause~%     ~s~%     ~s~%     ~s"
			       bindings goal elem)))))
      (let ((form (cons 'list (mapcar #'rec goal))))
	(if guards
	    `(guard ,form
		    (and ,@(loop for (k . v) in guards
			      collect `(equal ,k ,v))))
	    form)))))

(defun goal->lookup (base goal &key (bindings (make-hash-table)))
  (flet ((->ix (elem)
	   (cond ((and (variable? elem) (gethash elem bindings))
		  elem)
		 ((any-variables? elem)
		  nil)
		 (t elem))))
    (destructuring-bind (a b c) goal
      `(lookup ,base 
	       :a ,(->ix a) 
	       :b ,(->ix b)
	       :c ,(->ix c)))))

;; Fuck appending/collecting in and goals. You should just be pushing into a top-level variable. It'll simplify quite a few things.
(defmethod handle-goals ((goal-type (eql :and)) (loop-clause (eql :collect)) base goals collecting)
  (let ((bindings (make-hash-table))
	(final-res (gensym "FINAL-RES")))
    (labels ((single-goal (destruct lookup tail)
	       (with-gensyms (fact)
		 `(loop for ,fact in ,lookup 
		     do (match ,fact (,destruct ,(or tail `(push ,fact ,final-res)))))))
	     (rec (goals)
	       ;; We want to generate the lookups first, because the bindings are going to be generated
	       ;; from the result of the lookup. Meaning, if the bindings are established in a given destruct clause,
	       ;; they won't be usable until the NEXT lookup. 
	       ;; Therefore, even though it isn't immediately obvious, this let is necessary
	       (let ((lookup (goal->lookup base (first goals) :bindings bindings)))
		 (single-goal 
		  (goal->optima-clause (first goals) :bindings bindings)
		  lookup
		  (if (null (cdr goals))
		      (when collecting `(push ,collecting ,final-res))
		      (rec (rest goals)))))))
      `(let ((,final-res nil))
	 ,(rec (rest goals))
	 (reverse ,final-res)))))

(defmethod handle-goals ((goal-type (eql :and)) (loop-clause (eql :do)) base goals collecting)
  (let ((bindings (make-hash-table)))
    (labels ((single-goal (destruct lookup tail)
	       (with-gensyms (fact)
		 `(loop for ,fact in ,lookup do (match ,fact (,destruct ,tail)))))
	     (rec (goals)
	       ;; We want to generate the lookups first, because the bindings are going to be generated
	       ;; from the result of the lookup. Meaning, if the bindings are established in a given destruct clause,
	       ;; they won't be usable until the NEXT lookup. 
	       ;; Therefore, even though it isn't immediately obvious, this let is necessary
	       (let ((lookup (goal->lookup base (first goals) :bindings bindings)))
		 (single-goal 
		  (goal->optima-clause (first goals) :bindings bindings)
		  lookup
		  (if (null (cdr goals))
		      collecting
		      (rec (rest goals)))))))
      (rec (rest goals)))))

(defmethod handle-goals ((goal-type (eql :or)) (loop-clause (eql :collect)) base goals collecting)
  (with-gensyms (fact res)
    `(loop for ,fact in ,(goal->lookup base '(nil nil nil))
	for ,res = (match ,fact ((or ,@(mapcar #'goal->optima-clause (cdr goals))) ,(or collecting fact)))
	when ,res ,loop-clause ,res)))

(defmethod handle-goals ((goal-type (eql :or)) (loop-clause (eql :do)) base goals collecting)
  (with-gensyms (fact)
    `(loop for ,fact in ,(goal->lookup base '(nil nil nil))
	do (match ,fact ((or ,@(mapcar #'goal->optima-clause (cdr goals))) ,collecting)))))

(defmethod handle-goals ((goal-type (eql :not)) (loop-clause (eql :collect)) base goals collecting)
  (with-gensyms (fact res)
    `(loop for ,fact in ,(goal->lookup base '(nil nil nil))
	for ,res = (match ,fact ((not (or ,@(mapcar #'goal->optima-clause (cdr goals)))) ,(or collecting fact)))
	when ,res ,loop-clause ,res)))

(defmethod handle-goals ((goal-type (eql :not)) (loop-clause (eql :do)) base goals collecting)
  (with-gensyms (fact)
    `(loop for ,fact in ,(goal->lookup base '(nil nil nil))
	do (match ,fact ((not (or ,@(mapcar #'goal->optima-clause (cdr goals)))) ,collecting)))))

(defmethod handle-goals (goal-type (loop-clause (eql :collect)) base goals collecting)
  (with-gensyms (fact res)
    `(loop for ,fact in ,(goal->lookup base goals) 
	for ,res = (match ,fact (,(goal->optima-clause goals) ,(or collecting fact)))
	when ,res ,loop-clause ,res)))

(defmethod handle-goals (goal-type (loop-clause (eql :do)) base goals collecting)
  (with-gensyms (fact)
    `(loop for ,fact in ,(goal->lookup base goals) 
	do (match ,fact (,(goal->optima-clause goals) ,collecting)))))

(defmacro for-all (goal-term &key in collect do)
  (assert (or (and collect (not do))
	      (and do (not collect))
	      (and (not collect) (not do))))
  (with-gensyms (base)
    (let ((template (replace-anonymous (or collect do))))
      `(let ((,base ,in))
	 ,(handle-goals (->key (first goal-term))
			(if do :do :collect)
			base goal-term template)))))
