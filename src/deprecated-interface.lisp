(in-package :parenscript)

(define-condition simple-style-warning (simple-condition style-warning)
  ())

(defun warn-deprecated (old-name &optional new-name)
  (warn 'simple-style-warning
        :format-control "~:@(~a~) is deprecated~:[.~;, use ~:@(~a~) instead~]"
        :format-arguments (list old-name new-name new-name)))

(defmacro defun-js (old-name new-name args &body body)
  `(defun ,old-name ,args
    ,(when (and (stringp (car body)) (< 1 (length body))) ; docstring
           (car body))
    (warn-deprecated ',old-name ',new-name)
    ,@body))

;;; DEPRECATED INTERFACE

(defmacro define-script-symbol-macro (name &body body)
  (warn-deprecated 'define-script-symbol-macro 'define-ps-symbol-macro)
  `(define-ps-symbol-macro ,name ,@body))

(defun js-equal (ps-form1 ps-form2)
  (warn-deprecated 'js-equal)
  (equalp ps-form1 ps-form2))

(defun-js js-compile compile-script (form)
  (compile-script form))

(defun-js js-compile-list compile-script (form)
  (compile-script form))

(defmacro defjsmacro (&rest args)
  (warn-deprecated 'defjsmacro 'defpsmacro)
  `(defpsmacro ,@args))

(defmacro js-inline (&rest body)
  (warn-deprecated 'js-inline 'ps-inline)
  `(js-inline* '(progn ,@body)))

(defun-js js-inline* ps-inline* (&rest body)
  (apply #'ps-inline* body))

(defmacro with-unique-js-names (&rest args)
  (warn-deprecated 'with-unique-js-names 'with-ps-gensyms)
  `(with-ps-gensyms ,@args))

(defun-js gen-js-name ps-gensym (&optional (prefix "_js_"))
  (ps-gensym prefix))

(defmacro js (&rest args)
  (warn-deprecated 'js 'ps)
  `(ps ,@args))

(defun-js js* ps* (&rest args)
  (apply #'ps* args))

(defun-js compile-script ps1* (ps-form &key (output-stream nil))
  "Compiles the Parenscript form PS-FORM into Javascript.
If OUTPUT-STREAM is NIL, then the result is a string; otherwise code
is output to the OUTPUT-STREAM stream."
  (format output-stream "~A" (ps1* ps-form)))

(defun-js symbol-to-js symbol-to-js-string (symbol)
  (symbol-to-js-string symbol))
