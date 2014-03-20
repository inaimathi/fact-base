# fact-base
#### Simple implementation of fact-base data storage for Common Lisp

### Requirements

`:fact-base` depends on `alexandria`, `anaphora`, `local-time`, `optima` and `cl-fad`. All of them installable via `quicklisp`.

### Example Usage

	; SLIME 2013-11-17
	CL-USER> (ql:quickload :fact-base)
	To load "fact-base":
	  Load 1 ASDF system:
	    fact-base
	; Loading "fact-base"
	
	(:FACT-BASE)
	CL-USER> (defpackage :demo (:use :cl :fact-base))
	#<PACKAGE "DEMO">
	CL-USER> (in-package :demo)
	#<PACKAGE "DEMO">
	DEMO> (defparameter *db* (make-fact-base))
	*DB*
	DEMO> (insert! (list 0 :message "This is a sample message") *db*)
	NIL
	DEMO> (insert! (list 1 :message "This is another one") *db*)
	NIL
	DEMO> (insert! (list 1 :author "Inaimathi") *db*)
	NIL
	DEMO> (insert! (list 2 :message "That second one was written by me. This one is a meta-message.") *db*)
	NIL
	DEMO> (insert! (list 2 :type :meta) *db*)
	NIL
	DEMO> (current *db*)
	((2 :TYPE :META)
	 (2 :MESSAGE "That second one was written by me. This one is a meta-message.")
	 (1 :AUTHOR "Inaimathi") (1 :MESSAGE "This is another one")
	 (0 :MESSAGE "This is a sample message"))
	DEMO> (history *db*)
	((@2014-03-20T09:47:19.622527-04:00 :INSERT (2 :TYPE :META))
	 (@2014-03-20T09:47:16.186383-04:00 :INSERT
	  (2 :MESSAGE
	   "That second one was written by me. This one is a meta-message."))
	 (@2014-03-20T09:47:08.834689-04:00 :INSERT (1 :AUTHOR "Inaimathi"))
	 (@2014-03-20T09:47:03.936936-04:00 :INSERT (1 :MESSAGE "This is another one"))
	 (@2014-03-20T09:46:04.771430-04:00 :INSERT
	  (0 :MESSAGE "This is a sample message")))
	DEMO> (write! *db*)
	"fb-1007"

*Interlude: The file `fb-1007` now contains*

	((5132 49564 771430000) :INSERT (0 :MESSAGE "This is a sample message"))
	((5132 49623 936936000) :INSERT (1 :MESSAGE "This is another one"))
	((5132 49628 834689000) :INSERT (1 :AUTHOR "Inaimathi"))
	((5132 49636 186383000) :INSERT
	 (2 :MESSAGE "That second one was written by me. This one is a meta-message."))
	((5132 49639 622527000) :INSERT (2 :TYPE :META))

*we now resume your regularly scheduled demo.*

	DEMO> (multi-insert!
	       '((:message "Getting sick of specifying IDs manually, so I'll use multi-insert!")
		 (:user "Inaimathi")
		 (:mood "Curious"))
	       *db*)
	3
	DEMO> (current *db*)
	((3 :MOOD "Curious") (3 :USER "Inaimathi")
	 (3 :MESSAGE
	  "Getting sick of specifying IDs manually, so I'll use multi-insert!")
	 (2 :TYPE :META)
	 (2 :MESSAGE "That second one was written by me. This one is a meta-message.")
	 (1 :AUTHOR "Inaimathi") (1 :MESSAGE "This is another one")
	 (0 :MESSAGE "This is a sample message"))
	DEMO> (delta *db*)
	((@2014-03-20T09:51:13.740572-04:00 :INSERT (3 :MOOD "Curious"))
	 (@2014-03-20T09:51:13.740565-04:00 :INSERT (3 :USER "Inaimathi"))
	 (@2014-03-20T09:51:13.740544-04:00 :INSERT
	  (3 :MESSAGE
	   "Getting sick of specifying IDs manually, so I'll use multi-insert!")))
	DEMO> (multi-insert!
	       '((:message "This next one will just append the delta to the file created last time.")
		 (:user "Inaimathi")
		 (:mood "Still curious"))
	       *db*)
	4
	DEMO> (write-delta! *db*)
	"fb-1007"

*Interlude 2: That same file now contains...*

	((5132 49564 771430000) :INSERT (0 :MESSAGE "This is a sample message"))
	((5132 49623 936936000) :INSERT (1 :MESSAGE "This is another one"))
	((5132 49628 834689000) :INSERT (1 :AUTHOR "Inaimathi"))
	((5132 49636 186383000) :INSERT
	 (2 :MESSAGE "That second one was written by me. This one is a meta-message."))
	((5132 49639 622527000) :INSERT (2 :TYPE :META))
	((5132 49873 740544000) :INSERT
	 (3 :MESSAGE
	  "Getting sick of specifying IDs manually, so I'll use multi-insert!"))
	((5132 49873 740565000) :INSERT (3 :USER "Inaimathi"))
	((5132 49873 740572000) :INSERT (3 :MOOD "Curious"))
	((5132 49927 302099000) :INSERT
	 (4 :MESSAGE
	  "This next one will just append the delta to the file created last time."))
	((5132 49927 302129000) :INSERT (4 :USER "Inaimathi"))
	((5132 49927 302139000) :INSERT (4 :MOOD "Still curious"))

*That file wasn't cleared, then re-written. The delta was just appended to the bottom. Note that both `write!` and `write-delta!` take an optional filename as a parameter, so it's possible to break a fact-base up across files or back it up in another location*

	DEMO> (multi-insert!
	       '((:message "Ok, lets do some lookups now")
		 (:user "Inaimathi")
		 (:mood "Thoughtful"))
	       *db*)
	5
	DEMO> (lookup *db* :a 0)
	((0 :MESSAGE "This is a sample message"))
	T
	DEMO> (lookup *db* :a 2)
	((2 :TYPE :META)
	 (2 :MESSAGE "That second one was written by me. This one is a meta-message."))
	T
	DEMO> (lookup *db* :b :nope)
	NIL
	NIL
	DEMO> (lookup *db* :b :user)
	((5 :USER "Inaimathi") (4 :USER "Inaimathi") (3 :USER "Inaimathi"))
	T
	DEMO> (lookup *db* :a 3)
	((3 :MOOD "Curious") (3 :USER "Inaimathi")
	 (3 :MESSAGE
	  "Getting sick of specifying IDs manually, so I'll use multi-insert!"))
	T
	DEMO> (lookup *db* :a 4)
	((4 :MOOD "Still curious") (4 :USER "Inaimathi")
	 (4 :MESSAGE
	  "This next one will just append the delta to the file created last time."))
	T
	DEMO> (lookup *db* :a 5)
	((5 :MOOD "Thoughtful") (5 :USER "Inaimathi")
	 (5 :MESSAGE "Ok, lets do some lookups now"))
	T
	DEMO> (lookup *db* :a 5 :b :user)
	WARNING: No relevant index found, traversing...
	
	((5 :USER "Inaimathi"))
	DEMO> (multi-insert!
	       '((:message "The index system is still naive. It only has separate indices on :a, :b and :c, you see, but it doesn't save itself the time by at least starting with the smallest known index yet.")
		 (:user "Inaimathi")
		 (:mood "Even more thoughtful"))
	       *db*)
	6
	DEMO> (multi-insert!
	       '((:message "I'm going to fix that. In fact...")
		 (:user "Inaimathi")
		 (:mood "Even more thoughtful"))
	       *db*)
	7
	DEMO> (lookup *db* :c "Even more thoughtful")
	
	((7 :MOOD "Even more thoughtful") (6 :MOOD "Even more thoughtful"))
	T
	DEMO> (insert! (list 6 :important nil))
	; Evaluation aborted on #<SB-INT:SIMPLE-PROGRAM-ERROR "invalid number of arguments: ~S" {1004460C83}>.
	DEMO> (insert! (list 6 :important nil) *db*)
	NIL
	DEMO> (multi-insert!
	       '((:message "Yes, you can add metadata after the fact.")
		 (:user "Inaimathi")
		 (:mood "Oh, right..."))
	       *db*)
	8
	DEMO> 

### Function documentation
##### `:make-fact-base`
##### `:file-name`
##### `:next-id!`
##### `:lookup`
##### `:select`
##### `:matching?`
##### `:insert!`
##### `:multi-insert!`
##### `:delete!`
##### `:project!`
##### `:project`
##### `:write-delta!`
##### `:write!`
##### `:update!`
##### `:load!`
