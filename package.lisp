;;;; package.lisp

(defpackage #:fact-base
  (:use #:cl #:optima)
  (:shadow #:delete)
  (:import-from #:alexandria #:with-gensyms #:if-let #:when-let)
  (:import-from #:local-time #:timestamp)
  (:export #:fact-base #:make-fact-base #:current #:delta #:total-entries #:fork-at #:rewind-by #:rewind-to
	   #:file-name #:next-id!
	   #:for-all #:lookup
	   #:multi-insert! #:insert-new! #:insert! #:delete! #:change!
	   #:index! #:write! #:load! #:base!))
