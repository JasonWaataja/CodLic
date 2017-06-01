;;;; license.lisp

(in-package #:codlic)

(defun make-license-table ()
  (let ((license-table (make-hash-table :test #'equalp)))
    ;; Some make-hash calls.
    license-table))

(defparameter *license-table* (make-license-table)
  "The table mapping license names to their files on the system.")
