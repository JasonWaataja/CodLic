;;;; license.lisp
;;;; Information about licenses and their locations on the filesystem.

(in-package #:codlic)

(defun find-license-path (license-file)
  "Uses *INSTALL-PREFIX* to find the full path to the license file with the
given filename. For example, gplv3 would expand to *INSTALL-PREFIX/share/gplv3."
  (merge-pathnames (make-pathname :directory '(:relative "share" "codlic" "licenses")
                                  :name license-file)
                   (uiop:ensure-directory-pathname *install-prefix*)))

(defun make-license-table ()
  (let ((license-table (make-hash-table :test #'equalp)))
    (flet ((add-default (name)
             (setf (gethash name license-table)
                   (find-license-path name)))
           (add-with-name (name license-file-name)
             (setf (gethash name license-table)
                   (find-license-path license-file-name))))
      (add-default "gplv3")
      (add-with-name "gpl" "gplv3")
      (add-default "mit"))
    license-table))

(defparameter *license-table* (make-license-table)
  "The table mapping license names to their files on the filesystem.")
