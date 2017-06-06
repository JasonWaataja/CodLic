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
    ("non-blank-first-line" :none nil)
    ("skip-file-on-error" :none nil)))

(defun assoc-equal (item alist)
  (assoc item alist :test #'equal))

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
  (acase (name-cons options
		    :test #'equal
		    :else-form (license-error "No license, use --license-name or --license-file."))
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
    (if (and opening-string closing-string continuation-string)
	(make-composite-comment-type (cdr opening-string)
				     (cdr closing-string)
				     (cdr continuation-string)
				     :blank-first-line
				     (not (assoc "non-blank-first-line"
						 options
						 :test #'equal)))
	(license-error "When using custom comment strings, --opening-comment-string, --closing-comment-string, and --continuation-comment-string are required."))))

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
  "My own walking func, since I don't want to learn the uiop one. This one
purposefully doesn't follow symlinks because I don't want licensing to happen
recursively. That's why it doesn't just use the directory* function."
  (loop for file in (uiop:directory-files dirpath)
     do (funcall func file))
  (loop for dir in (uiop:subdirectories dirpath) do
       (funcall func dir)
       (walk-directory dir func)))

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
		   :text "Failed to write to file."))))

(defun license-arg (arg options)
  "Licenses an argument based on the given options."
  (let ((license-file (get-license options))
	(file-list (get-file-list arg options)))
    (license-error-if-nil license-file "Couldn't deduce license file to use.")
    (license-error-if-nil file-list (format nil
					    "Couldn't find any files to license for arg \"~a\""
					    arg))
    (loop with license-lines = (read-file-lines license-file)
       initially
	 (license-error-if-nil license-lines
			       (format nil "Failed to read license file \"~a\""
				       license-file))
       for file in file-list
       do (handler-case (add-license-lines file license-lines options)
	    (license-error (err)
	      (restart-case (file-license-error file
						(license-error-text err))
		(skip-file () nil)))))))

(defun skip-file (err)
  (declare (ignore err))
  (let ((restart (find-restart 'skip-file)))
    (when restart (invoke-restart restart))))

(defun make-file-license-error-handler (opts)
  (lambda (err)
    (let ((restart (find-restart 'skip-file)))
      (when (and restart (assoc "skip-file-on-error"
				opts
				:test #'equal))
	(format *error-output*
		"Warning, failed to license file \"~a\": ~a~%"
		(file-license-error-file err)
		(license-error-text err))))))

(defmacro exactly-one ((one-form &optional none-form more-form) &rest vals)
  "Returns one-form if exactly one of vals is true, none-form if none are true,
and more-form if more than one are true."
  (let ((value (gensym))
	(true-count (gensym)))
    `(loop for ,value in (list ,@vals)
	count ,value into ,true-count
	if (> ,true-count 1) return ,more-form
	finally (return (if (= ,true-count 1)
			    ,one-form
			    ,none-form)))))

(defun check-has-license (opts)
  (when (and (assoc-equal "license-name" opts)
	     (assoc-equal "license-file" opts))
    (license-error "Cannot use both --license-name and --license-file"))
  (when (and (not (assoc-equal "license-name" opts))
	     (not (assoc-equal "license-file" opts)))
    (acons "license-name" "mit" opts)
    (format t "No license, using --license-name mit~%")))

(defun check-has-filetype (opts)
  (when (and (assoc-equal "filetype-language" opts)
	     (assoc-equal "filetype-regex" opts))
    (license-error "Cannot use both --filetype-language and --filetype-regex"))
  (when (and (not (assoc-equal "filetype-language" opts))
	     (not (assoc-equal "filetype-regex" opts)))
    (format t "No filetype, attempting to license every file.~%")))

(defun check-has-comment-type (opts)
  (exactly-one (nil
		(progn (acons "auto-detect-comment-type" "auto-detect-comment-type" opts)
		       (format t "No comment type, using --auto-detect-comment-type"))
		(license-error "Must use no more than one of --single-comment-string, composite comment options, or --auto-detect-comment-type"))
	       (assoc-equal "single-comment-string" opts)
	       (or (assoc-equal "opening-comment-string" opts)
		   (assoc-equal "closing-comment-string" opts)
		   (assoc-equal "continuation-comment-string" opts))
	       (assoc-equal "auto-detect-comment-type" opts)))

(defun verify-options (opts)
  "Makes sure the options are consistent."
  (check-has-license opts)
  (check-has-filetype opts)
  (check-has-comment-type opts))

(defun process-args (remaining-args opts)
  "Call after reading the options. For each argument in remaining-args, attempt
to license the file or files that it points to. The list, options, is the alist
of arguments and their values."
  (loop
     initially
       (handler-case (verify-options opts)
	 (license-error (err)
	   (format *error-output* "Invalid arguments.~%~a~%"
		   (license-error-text err))
	   (return nil)))
     for arg in remaining-args do
       (handler-case
	   (handler-bind ((file-license-error (make-file-license-error-handler opts)))
	     (license-arg arg opts))
	 (license-error (err) (format *error-output*
				      "Failure while processing argument \"~a\"~%~
~a~%Aborting.~%"
				      arg
				      (license-error-text err))
			(return nil)))
     finally (return t)))

(defun main (argv)
  (multiple-value-bind (remaining-args
			opts
			unknown-opts)
      (getopt:getopt argv *cmd-options*)
    (cond (unknown-opts (format *error-output*
				"Failed to parse option~p ~{\"~a\"~^, ~}~%"
				(length unknown-opts)
				unknown-opts)
			nil)
	  ((null remaining-args)
	   (format *error-output* "No files to license, exiting.~%")
	   nil)
	  (t (process-args remaining-args opts)))))
