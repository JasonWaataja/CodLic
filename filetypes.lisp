;;;; filetypes.lisp

(in-package #:codlic)

(defun make-filetypes-table ()
  (let ((filetypes-table (make-hash-table :test #'equalp)))
    (setf (gethash "c" filetypes-table) ".*\\.c")
    (setf (gethash "cpp" filetypes-table) ".*\\.cpp|.*\\.cc")
    (setf (gethash "lisp" filetypes-table) ".*\\.lisp")
    filetypes-table))

(defparameter *filetypes-table* (make-filetypes-table)
  "The table that maps language names to a filetype regex.")

(defun get-filetype-regex (filetype)
  "Takes the name of a filetype such as \"C\" and return a regex string that
  matches files of the given language. Returns nil if no such regex exists. This
  match is case insensitive."
  (multiple-value-bind (regex found)
      (gethash filetype *filetypes-table*)
    (if found regex nil)))

(defun regex-matches-p (regex target-string)
  "Returns whether or not the target-string as a whole matches the regex."
  (multiple-value-bind (begin end)
      (cl-ppcre:scan regex target-string)
    (cond ((not begin) nil)
          ((or (not (eql begin 0))
               (not (eql end (length target-string))))
           nil)
          (t t))))

(defun file-matches-language-p (pathspec filetype)
  "Checks if the file at pathspec matches the language specified by filetype."
  (let ((regex (get-filetype-regex filetype)))
    (if regex
        ;; Ensure it's not a pathname object.
        (regex-matches-p regex (namestring pathspec))
        nil)))
