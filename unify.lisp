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

(defun goal->destructuring-form (goal &key (bindings (make-hash-table)))
  (labels ((rec (elem)
	     (cond ((listp elem)
		    (mapcar #'rec elem))
		   ((or (eq '? elem) (not (variable? elem)))
		    (gensym))
		   ((and (variable? elem) (gethash elem bindings))
		    (gensym))
		   ((variable? elem)
		    (setf (gethash elem bindings) t)
		    elem)
		   (t (error "Somethings' up. goal->destructuring-form~%     ~s~%     ~s~%     ~s"
			     bindings goal elem)))))
    (mapcar #'rec goal)))

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

(defmethod handle-goals ((goal-type (eql 'and)) base goals collecting)
  (let ((bindings (make-hash-table)))
    (labels ((single-goal (destruct lookup tail)
	       `(loop for ,destruct in ,lookup ,@tail))
	     (rec (goals)
	       ;; We want to generate the lookups first, because the bindings are going to be generated
	       ;; from the result of the lookup. Meaning, if the bindings are established in a given destruct clause,
	       ;; they won't be usable until the NEXT lookup. 
	       ;; Therefore, even though it isn't immediately obvious, order matters in this let* form
	       (let* ((lookup (goal->lookup base (first goals) :bindings bindings))
		      (destruct (goal->destructuring-form (first goals) :bindings bindings)))
		 (if (null (cdr goals))
		     (single-goal destruct lookup `(collect ,collecting))
		     (single-goal destruct lookup `(append ,(rec (rest goals))))))))
      (rec (rest goals)))))

(defmethod handle-goals (goal-type base goals collecting)
  ;; Same story here as in handle-goals
  (let* ((bindings (make-hash-table))
	 (lookup (goal->lookup base goals :bindings bindings))
	 (destruct (goal->destructuring-form goals :bindings bindings)))
    `(loop for ,destruct in ,lookup collect ,collecting)))

(defmacro for-all (goal-term &key in collecting)
  (with-gensyms (base)
    (let ((template (replace-anonymous (or collecting `(list ,@(variables-in goal-term))))))
      `(let ((,base ,in))
	 ,(handle-goals (first goal-term) base goal-term template)))))
