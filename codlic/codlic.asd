;;;; codlic.asd

(asdf:defsystem #:codlic
  :description "Describe codlic here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:getopt #:cl-ppcre)
  :serial t
  :components ((:file "package")
	       (:file "conditions")
	       (:file "files")
	       (:file "filetypes")
	       (:file "license")
	       (:file "comments")
               (:file "codlic")))
