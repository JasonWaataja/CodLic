;;;; comments.lisp

(in-package #:codlic)

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
					   (if insert-space
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
					  (if insert-space
					      " "
					      "")
					  line)
			     output-lines)
	 (setf on-first-line nil)
       finally
	 (vector-push-extend (copy-seq closing-string) output-lines)
	 (return output-lines))))

(defun make-comment-types-table ()
  (let ((comment-types-table (make-hash-table :test #'equalp)))
    (setf (gethash "c" comment-types-table)
	  (make-composite-comment-type "/*"
				       " */"
				       " *"))
    (setf (gethash "cpp" comment-types-table)
	  (make-single-comment-type "//"))
    (setf (gethash "lisp" comment-types-table)
	  (make-single-comment-type ";;"))
  comment-types-table))

(defparameter *comment-types-table* (make-comment-types-table)
  "A table that maps languages names to commenting functions. It's case
  insensitive. The functions take an array of lines and return a new array of
  lines with the licensed lines.")
