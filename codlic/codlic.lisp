;;;; codlic.lisp

(in-package #:codlic)

(defparameter *cmd-options*
  '(("license-name" :required nil)
    ("license-file" :required nil)
    ("filetype-language" :required nil)
    ("filetype-regex" :required nil)
    ("comment-language" :required nil)
    ("auto-detect-comment-type" :none nil)
    ("single-comment-string" :required nil)
    ("opening-comment-string" :required nil)
    ("closing-comment-string" :required nil)
    ("continuation-comment-string" :required nil)
    ("non-blank-first-line" :none "non-blank-first-line")))

(defmacro acase ((cons-name alist &key else-form test) &rest test-forms)
  "Switch statement for alists. For each test form, of the form (test-form
  result-forms*), searches alist for test-form. In each case, if it is found,
  then result-forms for that case are evaluated and returned. If no case matches
  then else-form is evaluated and returned."
  ;; This is formed by going through the test-forms in reverse. Since a
  ;; consecutive cond won't work (assoc would have to be run twice), a series of
  ;; consecutive let and if statements must be used, but since the last one must
  ;; end in else-form it must be built in reverse.
  (setf test-forms (nreverse test-forms))
  (loop with final-form = '()
     initially
       (setf final-form
	     `(let ((,cons-name (assoc ,(first (first test-forms)) ,alist
				       :test ,test)))
		(if ,cons-name
		    (progn ,@(rest (first test-forms)))
		    ,else-form)))
     for test-form in (rest test-forms) do
       (setf final-form
	     `(let ((,cons-name (assoc ,(first test-form) ,alist
				       :test ,test)))
		(if ,cons-name
		    (progn ,@(rest test-form))
		    ,final-form)))
     finally (return final-form)))

(defun get-license (options)
  "Gets the correct license file for the given command line options"
  (acase (name-cons options :test #'equal)
    ("license-name" (gethash (cdr name-cons) *license-table*))
    ("license-file" (cdr name-cons))))

(defun comment-type-for-file (path)
  (loop for language being the hash-keys in *comment-types-table*
     using (hash-value comment-type)
     if (file-matches-language-p path language)
     return comment-type
     finally (return nil)))

(defun get-composite-comment-type (options)
  (let ((opening-string (assoc "opening-comment-string" options
			       :test #'equal))
	(closing-string (assoc "closing-comment-string" options
			       :test #'equal))
	(continuation-string (assoc "continuation-comment-string" options
				    :test #'equal)))
    (when (and opening-string closing-string continuation-string)
      (make-composite-comment-type (cdr opening-string)
				   (cdr closing-string)
				   (cdr continuation-string)
				   :blank-first-line
				   (not (assoc "non-blank-first-line"
					       options
					       :test #'equal))))))

(defun get-comment-type (file options)
  "Gets the correct comment type based on the current argumet and previously
passed arguments. This can change based on the arg if auto-detection is used."
  (acase (comment-cons options
		       :else-form (get-composite-comment-type options)
		       :test #'equal)
    ("comment-language" (gethash (cdr comment-cons) *comment-types-table*))
    ("auto-detect-comment-type" (comment-type-for-file file))
    ("single-comment-string" (make-single-comment-type (cdr comment-cons)))))

(defun walk-directory (dirpath func)
  (let ((wildpath (make-pathname :defaults (uiop:ensure-directory-pathname
					    dirpath)
				 :name :wild
				 :type :wild)))
    (loop for entry in (uiop:directory* wildpath)
       do (funcall func entry)
       when (uiop:directory-exists-p entry)
       do (walk-directory entry func))))

(defun should-license-p (file options)
  ;; If none of these options are present, license by default.
  (acase (license-cons options :else-form t :test #'equal)
    ("filetype-language" (file-matches-language-p file
						  (cdr license-cons)))
    ("filetype-regex" (regex-matches-p (cdr license-cons)
				       file))))

(defun get-file-list (arg options)
  "Gets the list of files to operate on for the given argument and options. If
it points to a file then the list just contains that file but if it is a
directory then it recursively finds files in it."
  (if (uiop:directory-exists-p arg)
      (let ((file-list '()))
	(walk-directory arg
			#'(lambda (entry)
			    (when (uiop:file-exists-p entry)
			      (push entry file-list))))
	(nreverse file-list))
      (list arg)))

(defun create-output-lines (input-lines license-lines comment-type)
  "Returns the array of lines to write to the file starting with the given
license lines, then a blank line, and finally the set of input lines. Both
input-lines and license-lines are arrays and the return value is an array."
  (concatenate 'vector
	       (funcall comment-type license-lines)
	       (list "")
	       input-lines))

(defun add-license-lines (file license-lines options)
  (when (should-license-p file options)
    (let ((comment-type
	   (fail-if-nil ((get-comment-type file options))
			'license-error
			:text "Failed to get comment type for file."))
	  (input-lines
	   (fail-if-nil ((read-file-lines file))
			'license-error
			:text "Failed to read input file.")))
      (fail-if-nil ((write-file-lines file
				      (create-output-lines
				       input-lines
				       license-lines
				       comment-type)))
		   'license-error
		   :text "Failed to write to file"))))

(defun license-arg (arg options)
  "Licenses an argument based on the given options."
  (let ((license-file (get-license options))
	(file-list (get-file-list arg options)))
    (fail-if-nil (license-file file-list)
		 'license-error
		 :text "Could not load license or file list for argument.")
    (loop with license-lines = (read-file-lines license-file)
       initially
	 (fail-if-nil (license-lines)
		      'license-error
		      :text "Failed to read license file.")
       for file in file-list
       do (add-license-lines file license-lines options))))

(defun one-element-p (list)
  "Returns whether the list contains exactly one element."
  (if (and list (not (cdr list)))
      t
      nil))

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
			 (license-error-text err))))))
	  (t (format *error-output*
		     "Failed to parse option~p ~{\"~a\"~}~%"
		     (one-element-p unknown-opts)
		     unknown-opts)))))
