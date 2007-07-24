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

;;; dual lisp/parenscript macro balderdash
;;; TODO: should probably move elsewhere ;;;
#+nil
(progn
(defmacro defmacro/js (name args &body body)
  "Define a Lisp macro and import it into the ParenScript macro environment."
  `(progn (defmacro ,name ,args ,@body)
	  (js:import-macros-from-lisp ',name)))

(defmacro defmacro+js (name args &body body)
  "Define a Lisp macro and a ParenScript macro in their respective
macro environments. This function should be used when you want to use
the same macro in both Lisp and ParenScript, but the 'macroexpand' of
that macro in Lisp makes the Lisp macro unsuitable to be imported into
the ParenScript macro environment."
  `(progn (defmacro ,name ,args ,@body)
    (defscriptmacro ,name ,args ,@body)))

(defun import-macros-from-lisp (&rest names)
  "Import the named Lisp macros into the ParenScript macro environment."
  (dolist (name names)
    (let ((name name))
      (undefine-js-special-form name)
      (setf (get-macro-spec name *script-macro-toplevel*)
            (cons nil (lambda (&rest args)
                        (macroexpand `(,name ,@args))))))))

(defmacro js-file (&rest body)
  `(html
    (:princ
     (js ,@body))))

(defmacro js-script (&rest body)
  `((:script :type "text/javascript")
    (:princ (format nil "~%// <![CDATA[~%"))
    (:princ (js ,@body))
    (:princ (format nil "~%// ]]>~%"))))

(defmacro js-inline (&rest body)
  `(js-inline* '(progn ,@body)))

(defmacro js-inline* (&rest body)
  "Just like JS-INLINE except that BODY is evaluated before being
converted to javascript."
  `(concatenate 'string "javascript:"
    (string-join (js-to-statement-strings (compile-script-form (list 'progn ,@body)) 0) " ")))
)