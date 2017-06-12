;;;; codlic.asd

(asdf:defsystem #:codlic
  :description "Describe codlic here"
  :author "Jason Waataja <jasonswaataja@gmail.com>"
  :license "GPLv3"
  :depends-on (#:getopt #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "parameters")
               (:file "conditions")
               (:file "files")
               (:file "filetypes")
               (:file "license")
               (:file "search")
               (:file "codlic")))
