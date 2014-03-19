;;;; package.lisp

(defpackage #:fact-base
  (:use #:cl #:optima)
  (:shadow #:delete)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:anaphora #:awhen #:aif #:it)
  (:export :make-fact-base :matching? :current :history :last-saved :_
	   :file-name :next-id! 
	   :select :lookup :insert! :multi-insert! :delete! :project! :project
	   :write! :write-delta! :load! :index!))

