;;;; package.lisp

(defpackage #:fact-base
  (:use #:cl #:optima)
  (:shadow #:delete)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:anaphora #:awhen #:aif #:it)
  (:import-from #:split-sequence #:split-sequence)
  (:export :fact-base :matching?
	   :file-name :next-id! 
	   :select :insert! :multi-insert! :delete! :project! :project
	   :write! :update! :load!))

