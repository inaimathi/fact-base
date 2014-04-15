;;;; fact-base.asd

(asdf:defsystem #:fact-base
  :serial t
  :description "Simple implementation of fact-base data storage for Common Lisp"
  :author "Inaimathi <leo.zovic@gmail.com> (I'm sorry)"
  :license "AGPL3"
  :depends-on (#:alexandria #:anaphora #:local-time #:optima #:cl-fad)
  :components ((:file "package")
	       (:file "util")
	       (:file "fact-base")
               (:file "index")
	       (:file "unify")))

