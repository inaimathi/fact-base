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
	DEMO> (insert! *db* (list 0 :message "This is a sample message"))
	NIL
	DEMO> (insert! *db* (list 1 :message "This is another one"))
	NIL
	DEMO> (insert! *db* (list 1 :author "Inaimathi"))
	NIL
	DEMO> (insert! *db* (list 2 :message "That second one was written by me. This one is a meta-message."))
	NIL
	DEMO> (insert! *db* (list 2 :type :meta))
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
	       *db*
	       '((:message "Getting sick of specifying IDs manually, so I'll use multi-insert!")
		     (:user "Inaimathi")
		     (:mood "Curious")))
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
	       *db*
	       '((:message "This next one will just append the delta to the file created last time.")
		     (:user "Inaimathi")
		     (:mood "Still curious")))
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
	       *db*
	       '((:message "Ok, lets do some lookups now")
		     (:user "Inaimathi")
		     (:mood "Thoughtful")))
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
	       *db*
	       '((:message "The index system is still naive. It only has separate indices on :a, :b and :c, you see, but it doesn't save itself the time by at least starting with the smallest known index yet.")
		     (:user "Inaimathi")
		     (:mood "Even more thoughtful")))
	6
	DEMO> (multi-insert!
	       *db*
	       '((:message "I'm going to fix that. In fact...")
		     (:user "Inaimathi")
		     (:mood "Even more thoughtful")))
	7
	DEMO> (lookup *db* :c "Even more thoughtful")
	
	((7 :MOOD "Even more thoughtful") (6 :MOOD "Even more thoughtful"))
	T
	DEMO> (insert! *db* (list 6 :important nil))
	NIL
	DEMO> (multi-insert!
	       *db*
	       '((:message "Yes, you can add metadata after the fact.")
		     (:user "Inaimathi")
		     (:mood "Oh, right...")))
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

### Notes

- file-name should have nothing to do with fact-base name; it should be the full path to a fact base. Its name should be the contents of the base-name fact
- Define a structure other than a list to store history.
	-Specifically, you want the ability to push to its end (since that's the only way we'll ever be doing it)
- Infrastructure to support starting from a non-empty fact-base
	- Could be useful for reducing memory use at the expense of history granularity
	- How much will it actually save?
	- This would require a separation of fact-bases and deltas (which we should probably have anyhow)
		- No, it wouldn't. You'd need a history label that looked like `(<timestamp> :initial <initial-fb>)`
		- That could then get processed by `load!` and similar, and ignored when we're applying deltas, or when they don't appear at the beginning of a fact-base file
	- This is a tough decision. On the one hand, I don't want there to be gigabyte-large files for no reason (and I suspect keeping *all* history forever is a bit of overkill for most practical uses), but on the other hand, the only way to prevent that in general seems to be history pruning. Once history is mutable, we don't have very many strong guarantees left about anything. For the moment, leave as is. If it turns out that FBs are fucking huge, I'll revisit this.
