(in-package :parenscript)

;;; DEPRECATED INTERFACE ;;;
(defun js-equal (a b) (script-equal a b))

(defun js-compile (form)
  (compile-script form :output-spec :javascript))

(defun js-compile-list (form)
  (compile-script form :output-spec :javascript))

(defun js-gensym (&rest args)
  (apply #'script-gensym args))

(defmacro defjsmacro (name args &rest body)
  "Define a ParenScript macro, and store it in the toplevel ParenScript macro environment.

DEPRECATED"
  `(defscriptmacro ,name ,args ,@body))