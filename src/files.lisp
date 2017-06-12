;;;; files.lisp
;;;; File input and output.

(in-package #:codlic)

(defun read-file-lines (pathspec)
  "Reads the lines in the given file, returns a vector with them on success, NIL
on failure."
  (with-open-file (reader pathspec :if-does-not-exist nil)
    (if reader
        (loop with lines = (make-array 0
                                       :adjustable t
                                       :fill-pointer 0
                                       :element-type 'string)
           for line = (read-line reader nil)
           while line
           do (vector-push-extend line lines)
           finally (return lines)))))

(defun write-file-lines (pathspec lines-array)
  "Writes each line in the array of lines to the file at PATHSPEC. Overwrites
the file if it exists. Returns T on success, NIL on failure."
  (with-open-file (writer pathspec :direction :output :if-exists :supersede)
    (loop for line across lines-array
       do (write-line line writer)
       finally (return t))))

(defun walk-directory (dirpath func)
  "My own walking func, since I don't want to learn the UIOP one. This one
purposefully doesn't follow symlinks because I don't want licensing to happen
recursively. That's why it doesn't just use the DIRECTORY* function."
  (loop for file in (uiop:directory-files dirpath)
     do (funcall func file))
  (loop for dir in (uiop:subdirectories dirpath) do
       (funcall func dir)
       (walk-directory dir func)))
