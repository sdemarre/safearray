;;;; safearray.asd

(asdf:defsystem #:safearray
  :description "Describe safearray here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:iterate #:cffi #:alexandria #:trivial-garbage)
  :components ((:file "package")
	       (:file "constants")
	       (:file "macros")
	       (:file "com-error-codes")
               (:file "safearray")
	       (:file "safearrayclass")))

