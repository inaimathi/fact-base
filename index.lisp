(in-package #:fact-base)

(defclass index ()
  ((table :reader table :initform (make-hash-table :test 'equal))))

(defun make-index (indices)
  (let ((index (make-instance 'index)))
    (loop for ix in indices
       do (setf (gethash ix (table index))
		(make-hash-table :test 'equal)))
    index))

(defmethod indexed? ((state index) (ix-type symbol))
  (gethash ix-type (table state)))

(defun decide-index (&optional a b c)
  (cond ((and a b c) (list :abc a b c))
	((and a b) (list :ab a b))
	((and a c) (list :ac a c))
	((and b c) (list :bc b c))
	((and a) (list :a a))
	((and b) (list :b b))
	((and c) (list :c c))))

(defmethod format-index ((ix-type symbol) (fact list))
  (destructuring-bind (a b c) fact
    (case ix-type
      (:abc (list a b c))
      (:ab (list a b))
      (:ac (list a c))
      (:bc (list b c))
      (:a (list a))
      (:b (list b))
      (:c (list c)))))

(defmethod map-insert! ((facts list) (state index))
  (dolist (f facts) (insert! f state)))

(defmethod insert! ((fact list) (state index))
  (loop for ix being the hash-keys of (table state)
     for ix-table being the hash-values of (table state)
     do (push fact (gethash (format-index ix fact) ix-table))))

(defmethod delete! ((fact list) (state index))
  (loop for ix being the hash-keys of (table state)
     for ix-table being the hash-values of (table state)
     for formatted = (format-index ix fact)
     do (setf (gethash formatted ix-table) 
	      (remove fact (gethash formatted ix-table) :test #'equal :count 1))
     unless (gethash formatted ix-table) do (remhash formatted ix-table)))

;;;;; Show methods
;; Entirely for debugging purposes. 
;; Do not use in production. 
;; Seriously.
(defmethod show (thing &optional (depth 0))
  (format t "~a~a" (make-string depth :initial-element #\space) thing))

(defmethod show ((tbl hash-table) &optional (depth 0))
  (loop for k being the hash-keys of tbl
     for v being the hash-values of tbl
     do (format t "~a~5@a ->~%" 
		(make-string depth :initial-element #\space) k)
     do (show v (+ depth 8))
     do (format t "~%")))

(defmethod show ((ix index) &optional (depth 0))
  (show (table ix) depth))
