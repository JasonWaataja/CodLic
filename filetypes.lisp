;;;; filetypes.lisp
;;;; Information about filetypes and regexes to test for them.

(in-package #:codlic)

(defun default-comment-type (input-lines)
  "A comment type that does nothing."
  (copy-seq input-lines))

(defclass language ()
  ((filetype-regex :reader language-filetype-regex
                   :initarg :filetype-regex
                   :initform "$^"
                   :type 'string
                   :documentation "The regex that matches files for this language.")
   (comment-type :reader language-comment-type
                 :initarg :comment-type
                 :initform #'default-comment-type
                 :type 'function
                 :documentation "The function that creates comment lines.")))

(defun make-language (filetype-regex comment-type)
  "Creates a new langaugE"
  (make-instance 'language
                 :filetype-regex filetype-regex
                 :comment-type comment-type))

(defun make-single-comment-type (comment-string &optional (insert-space t))
  "Takes a comments-string such as \"#\" or \";;\" and returns a commenting
  function that comments using that string."
  (lambda (input-lines)
    (loop with output-lines = (make-array 0
                                          :adjustable t
                                          :fill-pointer 0
                                          :element-type 'string)
       for line across input-lines
       do (vector-push-extend (concatenate 'string
                                           comment-string
                                           (if (and insert-space
                                                    (not (zerop (length line))))
                                               " "
                                               "")
                                           line)
                              output-lines)
       finally (return output-lines))))

(defun make-composite-comment-type (opening-string
                                    closing-string
                                    continuation-string
                                    &key
                                      (insert-space t)
                                      (blank-first-line t))
  (lambda (input-lines)
    (loop with output-lines = (make-array 0
                                          :adjustable t
                                          :fill-pointer 0
                                          :element-type 'string)
       with on-first-line = t
       with line-count = (length input-lines)
       initially
         (when blank-first-line
           (vector-push-extend (copy-seq opening-string)
                               output-lines))
         (when (and (not blank-first-line)
                    (= line-count 0))
           (error 'license-error :text "Cannot have non-blank first line with no lines."))
       for line across input-lines do
         (vector-push-extend (concatenate 'string
                                          (if (and on-first-line (not blank-first-line))
                                              opening-string
                                              continuation-string)
                                          (if (and insert-space
                                                   (not (zerop (length line))))
                                              " "
                                              "")
                                          line)
                             output-lines)
         (setf on-first-line nil)
       finally
         (vector-push-extend (copy-seq closing-string) output-lines)
         (return output-lines))))

(defun make-languages-table ()
  "Creates the langauges table."
  (let ((language-table (make-hash-table :test #'equal)))
    (macrolet ((add-language-hashes (&body hash-forms)
                                    (loop for hash-form in hash-forms
                                       collect `(setf (gethash ,(first hash-form) language-table)
                                                      (make-language ,(second hash-form)
                                                                     ,(third hash-form)))
                                       into set-forms
                                       finally (return `(progn ,@set-forms)))))
      (add-language-hashes
        ("c" ".*\\.c" (make-composite-comment-type "/*"
                                                   " *"
                                                   " */"))
        ("cpp" ".*\\.cc|.*\\.cpp|.*\\.C" (make-single-comment-type "//"))
        ("lisp" ".*\\.lisp|.*\\.cl" (make-single-comment-type ";;"))
        ("java" ".*\\.java" (make-single-comment-type "//"))
        ("python" ".*\\.py" (make-single-comment-type "#"))
        ("javascript" ".*\\.js" (make-single-comment-type "//"))
        ("php" ".*\\.php" (make-single-comment-type "//"))
        ("perl" ".*\\.pl" (make-single-comment-type "#"))
        ("ruby" ".*\\.rb" (make-single-comment-type "#"))
        ("go" ".*\\.go" (make-single-comment-type "//"))
        ("bash" ".*\\.sh" (make-single-comment-type "#"))
        ("sh" ".*\\.sh" (make-single-comment-type "#"))
        ("rust" ".*\\.rs" (make-single-comment-type "//"))))
    language-table))

(defparameter *languages-table* (make-languages-table)
  "Information on various programming languages, including a regex to match
  files of that programming language and the instructions to create comments for
  that language.")

(defun get-filetype-regex (filetype)
  "Takes the name of a filetype such as \"C\" and return a regex string that
  matches files of the given language. Returns NIL if no such regex exists. This
  match is case insensitive."
  (multiple-value-bind (language found)
      (gethash filetype *languages-table*)
    (if found (language-filetype-regex language) nil)))

(defun regex-matches-p (regex target-string)
  "Returns whether or not TARGET-STRING as a whole matches the regex."
  (multiple-value-bind (begin end)
      (cl-ppcre:scan regex target-string)
    (cond ((not begin) nil)
          ((or (not (eql begin 0))
               (not (eql end (length target-string))))
           nil)
          (t t))))

(defun file-matches-language-p (pathspec filetype)
  "Checks if the file at PATHSPEC matches the language specified by FILETYPE."
  (let ((regex (get-filetype-regex filetype)))
    (if regex
        ;; Ensure it's not a pathname object.
        (regex-matches-p regex (namestring pathspec))
        nil)))

(defun get-comment-type-for-language (filetype)
  "Searches *LANGUAGES-TABLE* for FILETYPE. Returns the corresponding commenting
function if found, NIL otherwise."
  (multiple-value-bind (language found)
      (gethash filetype *languages-table*)
    (if found (language-comment-type language) nil)))
