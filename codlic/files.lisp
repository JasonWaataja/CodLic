;;;; files.lisp

(in-package #:codlic)

(defun read-file-lines (pathspec)
  "Reads the lines in the given file, returns a vector with them on success, nil
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
  "Writes each line in the array of lines to the file at pathspec. Overwrites
the file if it exists. Returns t on success, nil on failure."
  (with-open-file (writer pathspec :direction :output :if-exists :supersede)
    (loop for line across lines-array
       do (write-line line writer)
       finally (return t))))
