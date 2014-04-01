(in-package :fact-base)

(defclass meta-base ()
  ((file-name :accessor file-name :initarg :file-name)
   (fact-bases :accessor fact-bases :initarg :fact-bases 
	       :initform (make-hash-table :test 'equal))
   (indices :accessor indices :initarg :indices)))

(defmethod next-id! ((state meta-base))
  (format nil "branch-~a" (length (fact-bases state))))

(defun make-meta-base (&key (indices '(:a :b :c)) (file-name (cl-fad:pathname-as-directory (temp-file-name))))
  (let ((res (make-instance 'meta-base :file-name file-name :indices indices)))
    (new-fact-base! res :file-name "master" :indices indices)
    res))

(defmethod fact-base ((state meta-base) fact-base-name)
  (gethash fact-base-name (fact-bases state)))

(defmethod delete! ((state meta-base) (base fact-base))
  (when (fact-base state (file-name base))
    (delete-file (merge-pathnames (file-name base) (file-name state)))
    (remhash (file-name base) (fact-bases state))))

(defmethod write-delta! ((state meta-base) &key (file-name (file-name state)) (zero-delta? t))
  (loop for base being the hash-values of (fact-bases state)
     for fname = (merge-pathnames (file-name base) file-name)
     do (write-delta! base :file-name fname :zero-delta? zero-delta?)))

(defmethod write! ((state meta-base) &key (file-name (file-name state)) (zero-delta? t))
  (loop for base being the hash-values of (fact-bases state)
     for fname = (merge-pathnames (file-name base) file-name)
     do (write! base :file-name fname :zero-delta? zero-delta?)))

(defmethod load! ((base-type (eql :meta-base)) (file-name string) &key (indices '(:a :b :c)))
  (let ((res (make-meta-base :indices indices :file-name file-name)))
    (loop for file in (cl-fad:list-directory file-name)
       do (add-fact-base! res (load! :fact-base file)))))

(defmethod add-fact-base! ((state meta-base) (base fact-base))
  (unless (gethash (file-name base) (fact-bases state))
    (setf (gethash (file-name base) (fact-bases state)) base)))

(defmethod new-fact-base! ((state meta-base) &key (file-name (temp-file-name)) (indices '(:a :b :c)))
  (add-fact-base! state (make-fact-base :indices indices :file-name (merge-pathnames file-name (file-name state)))))

;; (defmethod branch! ((state meta-base) (base fact-base) &key (new-name (next-id! state)) branch-point)
;;   (unless (fact-base state new-name)
;;     (let ((f-name (merge-pathnames new-name (file-name state))))
;;       (write! base :file-name f-name)
;;       (setf (gethash new-name (fact-bases state)) (load! f-name)))))
