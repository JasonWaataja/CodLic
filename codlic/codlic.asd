;;;; codlic.asd

(asdf:defsystem #:codlic
  :description "Describe codlic here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:getopt)
  :serial t
  :components ((:file "package")
               (:file "codlic")))

