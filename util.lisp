(in-package #:fact-base)

(defun new-id (&optional prefix) 
  (intern (symbol-name (gensym prefix)) :keyword))

(defmethod traverse ((state hash-table) (ixes list))
  "Takes a hash-table and a list of indices.
Indexes into the hash-table recursively until it either runs out of indices or finds a nil.
Returns the deepest hash table it finds."
  (loop with cur = state
     for (ix . rest) on ixes while rest
     if (gethash ix cur) do (setf cur (gethash ix cur))
     else do (return (values nil nil))
     finally (return (values cur ix))))

(defmethod traverse! ((state hash-table) (ixes list))
  "As traverse, but creates new hash tables as it goes."
  (loop with cur = state
     for (ix . rest) on ixes while rest
     if (gethash ix cur) do (setf cur (gethash ix cur))
     else do (setf (gethash ix cur) (make-hash-table :test 'equal)
			     cur (gethash ix cur))
     finally (return (values cur ix))))

(defmethod deep-lookup ((state hash-table) (ixes list))
  (multiple-value-bind (res res?) (traverse state ixes)
    (if res?
	(gethash res? res)
	(values res res?))))

(defmethod deep-set! ((state hash-table) (ixes list) value)
  (multiple-value-bind (res ix) (traverse! state ixes)
      (setf (gethash ix res) value)))

(defmethod deep-push! ((state hash-table) (ixes list) value)
  (multiple-value-bind (res ix) (traverse! state ixes)
    (push value (gethash ix res))))
