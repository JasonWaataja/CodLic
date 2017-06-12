;;;; util.lisp
;;;; Utilities for codlic

(in-package #:codlic)

(defmacro add-hashes ((hashmap) &body hash-forms)
  "Takes expressions of the form \"(hash-key hash-value)\" in HAS-FORMS and for
  each one sets the value of the first value in HASHMAP to the value of the
  second."
  (loop for hash-form in hash-forms
     for hash-key = (first hash-form)
     for hash-value = (second hash-form)
     collect `(setf (gethash ,hash-key ,hashmap) ,hash-value) into set-forms
     finally (return `(progn ,@set-forms))))

(defun assoc-equal (item alist)
  (assoc item alist :test #'equal))

(defmacro acase ((cons-name alist &key else-form test) &rest test-forms)
  "Switch statement for alists. For each test form, of the form (test-form
  result-forms*), searches alist for test-form. In each case, if it is found,
  then TEST-FORMS for that case are evaluated and returned. If no case matches
  then ELSE-FORM is evaluated and returned."
  ;; This is formed by going through the TEST-FORMS in reverse. Since a
  ;; consecutive cond won't work (assoc would have to be run twice), a series of
  ;; consecutive LET and IF statements must be used, but since the last one must
  ;; end in ELSE-FORM it must be built in reverse.
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

(defun has-prefix (string prefix)
  "Returns if STRING begins with prefix."
  (let ((str-length (length string))
        (prefix-length (length prefix)))
    (if (< str-length prefix-length)
        nil
        (string= string prefix :end1 prefix-length :end2 prefix-length))))

(defmacro exactly-one ((one-form &optional none-form more-form) &rest vals)
  "Returns ONE-FORM if exactly one of VALS is true, NONE-FORM if none are true,
and MORE-FORM if more than one are true."
  (let ((value (gensym))
        (true-count (gensym)))
    `(loop for ,value in (list ,@vals)
        count ,value into ,true-count
        if (> ,true-count 1) return ,more-form
        finally (return (if (= ,true-count 1)
                            ,one-form
                            ,none-form)))))
