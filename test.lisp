(in-package #:fact-base)

(defparameter *base* (make-fact-base :indices '(:a :ab :abc)))
(insert! *base* (list 2 :whiskey :foxtrot))
(insert! *base* (list 1 :foxtrot :beta))
(insert! *base* (list 0 :whiskey :tango))
(insert! *base* (list 4 :tango :beta))
(insert! *base* (list 0 :b :c))
;; (show (index *base*))
(lookup *base* :a 0)
(lookup *base* :a 0 :b :whiskey)
(delete! *base* (list 0 :whiskey :tango))
;; (show (index *base*))
(delete! *base* (list 0 :b :c))
;; (show (index *base*))

(defmethod test-generate! (n)
  (loop repeat n
     do (multi-insert! 
	 *base* `((:number ,(random 100)) (:type :digit) 
		  (:time ,(get-universal-time)) 
		  (:user ,(nth (random 4) '("Inaimathi" "Anon" "Someone Else" "Albert" "Beatrice" "Charles" "Daria")))))))

(test-generate! 10000)

(for-all (and (?id :number 62) (?id :user ?name)) :in *base* :collecting ?name)
