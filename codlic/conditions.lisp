;;;; conditions.lisp

(in-package #:codlic)

(define-condition license-error (error)
  ((text :initarg :text :reader license-error-text)))

(defmacro fail-if-nil ((&rest forms) error-type &rest args)
  "Checks each form in order. If any is nil, then throw an error constructed
with error-type and args. For example (fail-if-nil (t) 'my-error :text
\"my-text\"). Returns the values of the forms if no condition is signalled."
  (let ((values (gensym))
	(current-value (gensym)))
    (loop for form in forms
       with new-forms = '()
       do (push `(let ((,current-value ,form))
		   (if ,current-value
		       (push ,current-value ,values)
		       (error ,error-type ,@args)))
		new-forms)
       finally
	 (setf new-forms (nreverse new-forms))
	 (return `(let ((,values '()))
			  ,@new-forms
			  (values-list (nreverse ,values)))))))

(defun license-error (&optional text)
  (error 'license-error :text text))

(defun license-error-if-nil (expr &optional text)
  (fail-if-nil (expr)
	       'license-error
	       :text text))
