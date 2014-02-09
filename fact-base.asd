;;;; fact-base.asd

(asdf:defsystem #:fact-base
  :serial t
  :description "Describe fact-base here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:anaphora #:local-time #:optima #:split-sequence #:cl-fad)
  :components ((:file "package")
               (:file "fact-base")))

