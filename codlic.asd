;;;; codlic.asd

(asdf:defsystem #:codlic
  :description "Describe codlic here"
  :author "Jason Waataja <jasonswaataja@gmail.com>"
  :license "GPLv3"
  :homepage "https://github.com/JasonWaataja/codlic"
  :version "0.1.3"
  :depends-on (#:getopt #:cl-ppcre)
  :pathname "src"
  :components ((:file "package")
               (:file "util")
               (:file "parameters")
               (:file "conditions")
               (:file "files")
               (:file "filetypes")
               (:file "license")
               (:file "search")
               (:file "codlic")
               (:file "main")))
