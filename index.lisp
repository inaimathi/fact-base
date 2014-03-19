(in-package #:fact-base)

(defclass index ()
  ((table :reader table :initform (make-hash-table :test 'equal))))

(defun make-index (indices)
  (let ((index (make-instance 'index)))
    (loop for ix in indices
       do (setf (gethash ix (table index))
		(make-hash-table :test 'equal)))
    index))

(defmethod deep-lookup ((state index) (ixes list))
  (deep-lookup (table state) ixes))

(defmethod deep-set! ((state index) (ixes list) value)
  (deep-set! (table state) ixes value))

(defmethod deep-push! ((state index) (ixes list) value)
  (deep-push! (table state) ixes value))

(defmethod indexed? ((state index) (ix-type symbol))
  (gethash ix-type (table state)))

(define-index a)
(define-index b)
(define-index c)
(define-index a b)
(define-index a c)
(define-index b c)

;;;;; The lookup interface
(defun decide-index (&optional a b c)
  (cond ((and a b) 
	 (list :ab a b))
	((and a c)
	 (list :ac a c))
	((and b c)
	 (list :bc b c))
	(a (list :a a))
	(b (list :b b))
	(c (list :c c))))

(defmethod format-index ((ix-type symbol) (fact list))
  (case ix-type
    (:ab (list ix-type (first fact) (second fact)))
    (:ac (list ix-type (first fact) (third fact)))
    (:bc (list ix-type (second fact) (third fact)))))

(defmethod map-insert! ((facts list) (state index))
  (dolist (f facts) (insert! f state)))

(defmethod insert! ((fact list) (state index))
  (destructuring-bind (a b c) fact
    (loop for ix being the hash-keys of (table state)
       do (deep-push! state ??? fact))))

(defmethod remove! ((fact list) (state index))
  (loop for ix being the hash-keys of (table state)
     do (remove-ix ix state fact)))

;;;;; Show methods
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
