;;;; codlic.lisp

(in-package #:codlic)

(defun license-arg (arg options))

(defparameter *cmd-options*
  '(("license-name" :required nil)))

(defun get-license (options)
  "Gets the correct license file for the given command line options"
  nil)

(defun get-comment-type (arg options)
  "Gets the correct comment type based on the current argumet and previously
passed arguments. This can change based on the arg if auto-detection is used."
  nil)

(defun walk-directory (dirpath func)
  (let ((wildpath (make-pathname :defaults (uiop:ensure-directory-pathname
					    dirpath)
				 :name :wild
				 :type :wild)))
    (loop for entry in (uiop:directory* wildpath)
       do (funcall func entry)
       when (uiop:directory-exists-p entry)
       do (walk-directory entry func))))

(defun get-file-list (arg options)
  "Gets the list of files to operate on for the given argument and options. If
it points to a file then the list just contains that file but if it is a
directory then it recursively finds files in it."
  (cond ((uiop:file-exists-p arg) (list arg))
	((uiop:directory-exists-p arg)
	 (let ((file-list '()))
	   (walk-directory arg
			  #'(lambda (entry)
			      (when (uiop:file-exists-p entry)
				(push entry file-list))))
	   (nreverse file-list)))
	(t nil)))

(defun license-arg (arg options)
  "Licenses an argument based on the given options."
  (let ((license-type (get-license options))
	(comment-type (get-comment-type arg options))
	(file-list (get-file-list arg options)))
    (loop for file in file-list
       for lines = (read-file-lines file)
       when lines do
	 (write-file-lines file
			   (funcall comment-type license-type lines)))))

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
