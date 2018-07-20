;;;; main.lisp

(in-package #:codlic)

(defun main (argv)
  (multiple-value-bind (remaining-args
                        opts
                        unknown-opts)
      (getopt:getopt (cdr argv)
                     *cmd-options*)
    (cond (unknown-opts (format *error-output*
                                "Failed to parse option~p ~{\"~a\"~^, ~}~%"
                                (length unknown-opts)
                                unknown-opts)
                        nil)
          (t (process-args remaining-args opts)))))
