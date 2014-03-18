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
	     (push c (gethash b (gethash a ix)))))
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
	     (awhen (gethash a ix)
	       (when (gethash b it)
		 (setf (gethash b it) (remove c (gethash b it) :test #'equal)))
	       (unless (car (gethash b it))
		 (remhash b it))
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

(defmethod index-internal ((state index) (lookup function) (reorder function) a &optional b)
  (multiple-value-bind (res res?) (gethash a (funcall lookup state))
    (when res?
      (if b
	  (loop for elem in (gethash b res)
	     collect (funcall reorder (list a b elem)))
	  (loop for k being the hash-keys of res
	     append (loop for elem in (gethash k res)
		       collect (funcall reorder (list a k elem))))))))

(defmacro define-index (&rest order)
  (let ((args (butlast order))
	(ix-name (intern (format nil "~{~a~}" order)))
	(ix-type (intern (format nil "~{~a~}" (butlast order)) :keyword)))
    `(defmethod index-by ((lookup (eql ,ix-type)) (state index) ,(first order) &optional ,(second order))
       (index-internal state #',ix-name (reorder ,@order) ,@args))))

(define-index a b c)
(define-index b a c)
(define-index b c a)
(define-index c a b)
(define-index c b a)

;;;;; Test forms
;; (insert! (list 2 :whiskey :foxtrot) fb)
;; (insert! (list 1 :foxtrot :beta) fb)
;; (insert! (list 0 :whiskey :tango) fb)
;; (insert! (list 4 :tango :beta) fb)
;; (insert! (list 0 :b :c) fb)
