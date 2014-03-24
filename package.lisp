;;;; package.lisp

(defpackage #:fact-base
  (:use #:cl #:optima)
  (:shadow #:delete)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:anaphora #:awhen #:aif #:it)
  (:export :fact-base :make-fact-base :current :history :delta :_
	   :file-name :next-id! 
	   :lookup :select :matching? 
	   :multi-insert! :insert-new! :insert! :delete! 
	   :index! :project! :project
	   :write! :write-delta! :load!))

