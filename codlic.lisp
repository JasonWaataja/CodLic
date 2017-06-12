;;;; codlic.lisp
;;;; Main file for codlic, inserts license texts at the beginning of files.

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
    ("skip-file-on-error" :none nil)
    ("skip-shebang" :none nil)
    ("print-license" :none nil)
    ("license-replace" :required nil)))

(defun get-license (options)
  "Gets the correct license file for the given command line options"
  (acase (name-cons options
                    :test #'equal
                    :else-form (license-error "No license, use --license-name or --license-file."))
         ("license-name" (gethash (cdr name-cons) *license-table*))
         ("license-file" (cdr name-cons))))

(defun comment-type-for-file (path)
  (loop for language being the hash-values in *languages-table*
     if (regex-matches-p (language-filetype-regex language) path)
     return (language-comment-type language)
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
         ("comment-language" (get-comment-type-for-language (cdr comment-cons)))
         ("auto-detect-comment-type" (comment-type-for-file file))
         ("single-comment-string" (make-single-comment-type (cdr comment-cons)))))

(defun should-license-p (file options)
  ;; If none of these options are present, license anything by default.
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

(defun has-shebang (first-line)
  "Returns whether or not FIRST-LINE begins with \"#!\""
  (has-prefix first-line "#!"))

(defun create-output-lines (input-lines license-lines comment-type opts)
  "Returns the array of lines to write to the file starting with the given
license lines, then a blank line, and finally the set of input lines. Both
INPUT-LINES and LICENSE-LINES are arrays and the return value is an array."
  (if (and (assoc-equal "skip-shebang" opts)
           (plusp (length input-lines))
           (has-shebang (aref input-lines 0)))
      (let ((first-line (aref input-lines 0))
            (rest-lines (subseq input-lines 1)))
        (concatenate 'vector
                     (list first-line)
                     (list "")
                     (funcall comment-type license-lines)
                     (list "")
                     rest-lines))
      (concatenate 'vector
                   (funcall comment-type license-lines)
                   (list "")
                   input-lines)))

(defun add-license-lines (file license-lines options)
  "Adds LICENSE-LINES to the lines in FILE and writes back to it based on
OPTIONS. May signal a `license-error'."
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
                                       comment-type
                                       options)))
                   'license-error
                   :text "Failed to write to file."))))

(defun read-license-lines (license-file opts)
  "Essentially calls READ-FILE-LINES to read the lines of LICENSE-FILE and
return them. Also makes the necessary string replacements based on OPTS."
  (let ((license-lines (read-file-lines license-file))
        (replace-opt (assoc-equal "license-replace" opts)))
    (when replace-opt
      (loop for replace-pair in (parse-search-replace-string (cdr replace-opt)) do
           (when (string= (car replace-pair) "")
             (license-error "Attempting to replace empty string."))
           (setf license-lines
                 (replace-lines license-lines
                                (car replace-pair)
                                (cdr replace-pair)))))
    license-lines))

(defun license-arg (arg options)
  "Licenses an argument based on the given options."
  (let ((license-file (get-license options))
        (file-list (get-file-list arg options)))
    (license-error-if-nil license-file "Couldn't deduce license file to use.")
    (license-error-if-nil file-list (format nil
                                            "Couldn't find any files to license for arg \"~a\""
                                            arg))
    (loop with license-lines = (read-license-lines license-file options)
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

(defun check-has-license (opts)
  "Returns new version of OPTS, is destructive."
  (when (and (assoc-equal "license-name" opts)
             (assoc-equal "license-file" opts))
    (license-error "Cannot use both --license-name and --license-file"))
  (when (and (not (assoc-equal "license-name" opts))
             (not (assoc-equal "license-file" opts)))
    (setf opts (acons "license-name" "mit" opts))
    (format t "No license, using --license-name mit~%"))
  opts)

(defun check-has-filetype (opts)
  "Returns new version of OPTS, is destructive."
  (when (and (assoc-equal "filetype-language" opts)
             (assoc-equal "filetype-regex" opts))
    (license-error "Cannot use both --filetype-language and --filetype-regex"))
  (when (and (not (assoc-equal "filetype-language" opts))
             (not (assoc-equal "filetype-regex" opts)))
    (format t "No filetype, attempting to license every file.~%"))
  opts)

(defun check-has-comment-type (opts)
  "Returns new value of OPTS, is destructive."
  (exactly-one (nil
                (progn (setf opts
                             (acons "auto-detect-comment-type" nil opts))
                       (format t "No comment type, using --auto-detect-comment-type~%"))
                (license-error "Must use no more than one of --single-comment-string, composite comment options, or --auto-detect-comment-type"))
               (assoc-equal "single-comment-string" opts)
               (or (assoc-equal "opening-comment-string" opts)
                   (assoc-equal "closing-comment-string" opts)
                   (assoc-equal "continuation-comment-string" opts))
               (assoc-equal "auto-detect-comment-type" opts))
  opts)

(defun verify-options (opts)
  "Makes sure OPTS are consistent. Returns correct options if possible"
  (setf opts (check-has-license opts))
  (setf opts (check-has-filetype opts))
  (setf opts (check-has-comment-type opts))
  opts)

(defun print-license-file (license-file)
  "Returns T on success, NIL on failure."
  (let ((license-lines (read-file-lines license-file)))
    (if license-lines
        (loop for line across license-lines
           do (format t "~a~%" line)
           finally (return t))
        nil)))

(defun print-license (opts)
  "Returns T on success, NIL on failure."
  (let ((license (handler-case (get-license opts)
                   (license-error (err)
                     (format *error-output* "Cannot print license:~%~a~%"
                             (license-error-text err))
                     nil))))
    (if license
        (print-license-file license)
        nil)))

(defun process-args (remaining-args opts)
  "Call after reading the options. For each argument in REMAINING-ARGS, attempt
to license the file or files that it points to. The list, OPTS, is the alist of
arguments and their values."
  (loop
     initially
       (when (assoc-equal "print-license" opts)
         (return (print-license opts)))
       (handler-case (setf opts (verify-options opts))
         (license-error (err)
           (format *error-output* "Invalid arguments.~%~a~%"
                   (license-error-text err))
           (return nil)))
       (when (null remaining-args)
         (format *error-output* "No files to license, exiting.~%")
         (return nil))
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
          (t (process-args remaining-args opts)))))
