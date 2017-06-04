;;;; parameters.lisp

(in-package #:codlic)

(defmacro define-program-parameter (var default-val &optional doc)
  "Define a program parameter that is meant to be defined in a makefile. In the
  makefile, run cl-build with \"-e (defparameter *my-param* my-value)\". You can
  give this variable a default value if it is not defined in the makefile."
  (if (boundp var)
      nil
      `(defparameter ,var ,default-val ,doc)))

(define-program-parameter *install-prefix* "/usr/local"
  "The place to search for files that were installed.")
