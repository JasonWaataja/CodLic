;;;; search.lisp
;;;; Finding and replacing strings in arrays of lines.

(defun replace-all (string part replacement &key (test #'char=))
  "From the Common Lisp Cookbook, replaces instances of part with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defun replace-lines (lines part replacement)
  "Goes through each line in the array LINES and replaces all instances of PART
with REPLACEMENT."
  (map 'vector
       (lambda (line)
         (replace-all line part replacement))
       lines))

(defun split-string (string &optional (delimiter ":"))
  "Returns list of new strings."
  (loop with string-length = (length string)
     with delimiter-length = (length delimiter)
     with i = 0
     while (< i string-length)
     if (char= (char string i) #\\)
     do (incf i 2)
     else if (and (< (+ i delimiter-length) string-length)
             (string= string delimiter :start1 i :end1 (+ i delimiter-length)))
     collect (subseq string 0 i) into strings
     and do
       (setf string (subseq string (+ i delimiter-length))
             i 0
             string-length (length string))
     else do
       (incf i)
     finally (return (append strings (list (copy-seq string))))))

(defun parse-search-replace-string (control-string)
  "Parses CONTROL-STRING and returns an alist of strings to find and
replace. The format of CONTROL-STRING is a colon-separated list of expressions
with a search and replace part. The search and replace parts of a string are
separated by a forward slash. All forward slashes and colons that aren't meant
to be used as splitters should be escapped with a backslash. To insert a
backslash, use two backslashes."
  (let ((search-replace-forms (split-string control-string)))
    (loop for form in search-replace-forms
       for subforms = (split-string form "/")
       do (format t "~a~%" subforms)
       ;; Yes, I know this is slow.
       when (= (length subforms) 2)
       collect (cons (first subforms) (second subforms)))))
