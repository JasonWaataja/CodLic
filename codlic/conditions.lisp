;;;; conditions.lisp

(in-package #:codlic)

(define-condition license-error (error)
  ((text :initarg :text :reader license-error-text)))
