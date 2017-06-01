;;;; codlic.lisp

(in-package #:codlic)

(defun license-arg (arg options))

(defparameter *cmd-options*
  '(("license-name" :required nil)))

(defun license-arg (arg options))

(defun main (argv)
  (multiple-value-bind (remaining-args
			opts
			unknown-opts)
      (getopt:getopt argv *cmd-options*)
    (cond ((null unknown-opts)
	   (dolist (arg remaining-args)
	     (handler-case (license-arg arg opts)
	       (license-error (err)
		 (format *error-output* "Failed to license ~a~%~a~%"
			 arg
			 (license-error-text err))))
	     (license-arg arg opts)))
	  (t (format *error-output*
		     "Failed to parse options ~{~a~}~%"
		     unknown-opts)))))
