(in-package #:fact-base)

(defclass index ()
  ((abc :reader abc :initarg :abc :initform (make-hash-table :test 'equal))
   (bac :reader bac :initarg :bac :initform (make-hash-table :test 'equal))
   (bca :reader bca :initarg :bca :initform (make-hash-table :test 'equal))
   (cab :reader cab :initarg :cab :initform (make-hash-table :test 'equal))
   (cba :reader cba :initarg :bac :initform (make-hash-table :test 'equal))))

;;;;; Insertion/deletion
(defmethod insert! ((fact list) (state index))
  (macrolet ((push! (a b c) 
	       `(insert-ix 
		 ,a ,b ,c
		 (,(intern (format nil "~a~a~a" a b c)) state))))
    (flet ((insert-ix (a b c ix)
	     (unless (gethash a ix)
	       (setf (gethash a ix) (make-hash-table :test 'equal)))
	     (setf (gethash b (gethash a ix)) c)))
      (destructuring-bind (a b c) fact
	(push! a b c)
	(push! b a c)
	(push! b c a)
	(push! c a b)
	(push! c b a))
      state)))

(defmethod remove! ((fact list) (state index))
  (macrolet ((rem! (a b c) 
	       `(remove-ix 
		 ,a ,b ,c
		 (,(intern (format nil "~a~a~a" a b c)) state))))
    (flet ((remove-ix (a b c ix)
	     (declare (ignore c))
	     (when (gethash a ix)
	       (remhash b (gethash a ix))
	       (when (= 0 (hash-table-count (gethash a ix)))
		 (remhash a ix)))))
      (destructuring-bind (a b c) fact
	(rem! a b c)
	(rem! b a c)
	(rem! b c a)
	(rem! c a b)
	(rem! c b a))
      state)))

;;;;; Indexing (and related utility operations)
(defmacro reorder (fst snd thd)
  (with-gensyms (res)
    `(lambda (,res)
       (destructuring-bind (,fst ,snd ,thd) ,res
	 (list a b c)))))

(defmethod index-internal ((state index) (lookup function) (reorder function) &optional a b)
  (let ((res-a (gethash a (funcall lookup state))))
    (if b
	(list (funcall reorder (list a b (gethash b res-a))))
	(loop for k being the hash-keys of res-a
	   for v being the hash-values of res-a
	   collect (funcall reorder (list a k v))))))

(defmacro define-index (&rest order)
  (let ((args (butlast order))
	(ix-name (intern (format nil "~{~a~}" order)))
	(ix-type (intern (format nil "~{~a~}" (butlast order)) :keyword)))
    `(defmethod index ((state index) (lookup (eql ,ix-type)) &optional ,@args)
       (index-internal state #',ix-name (reorder ,@order) ,@args))))

(define-index a b c)
(define-index b a c)
(define-index b c a)
(define-index c a b)
(define-index c b a)
