;;;; package.lisp

(defpackage #:fact-base
  (:use #:cl #:optima)
  (:shadow #:delete)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:anaphora #:awhen #:aif #:it)
  (:export :fact-base :make-fact-base :current :history :delta :_

	   :meta-base :make-meta-base :add-fact-base! :branch!
	   :new-fact-base! :indices :list-bases :get-base

	   :file-name :next-id!
	   :for-all :lookup 
	   :multi-insert! :insert-new! :insert! :delete! 
	   :index! :project! :project :history-slice
	   :write! :write-delta! :load!))

