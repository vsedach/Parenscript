(in-package #:parenscript)
(in-readtable :parenscript)

(define-condition simple-style-warning (style-warning simple-warning)
  ())

(defun warn-deprecated (old-name &optional new-name)
  (unless *suppress-deprecation*
    (warn 'simple-style-warning
          :format-control "~:@(~a~) is deprecated~:[.~;, use ~:@(~a~) instead~]"
          :format-arguments (list old-name new-name new-name))))

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

(defun-js gen-js-name ps-gensym (&optional (prefix "_JS_"))
  (ps-gensym prefix))

(defmacro js (&rest args)
  (warn-deprecated 'js 'ps)
  `(ps ,@args))

(defun-js js* ps* (&rest args)
  (apply #'ps* args))

(defun-js compile-script ps* (ps-form &key (output-stream nil))
  "Compiles the Parenscript form PS-FORM into Javascript.
If OUTPUT-STREAM is NIL, then the result is a string; otherwise code
is output to the OUTPUT-STREAM stream."
  (format output-stream "~A" (ps* ps-form)))

(defun-js symbol-to-js symbol-to-js-string (symbol)
  (symbol-to-js-string symbol))

(defmacro defmacro/ps (name args &body body)
  (warn-deprecated 'defmacro/ps 'defmacro+ps)
  `(progn (defmacro ,name ,args ,@body)
          (import-macros-from-lisp ',name)))

(defmacro defpsmacro-deprecated (old new)
  `(defpsmacro ,old (&rest args)
     (warn-deprecated ',old ',new)
     (cons ',new args)))

(defpsmacro-deprecated slot-value getprop)
(defpsmacro-deprecated === eql)
(defpsmacro-deprecated == equal)
(defpsmacro-deprecated % rem)
(defpsmacro-deprecated concat-string stringify)

(defpsmacro !== (&rest args)
  (warn-deprecated '!==)
  `(not (eql ,@args)))

(defpsmacro != (&rest args)
  (warn-deprecated '!=)
  `(not (equal ,@args)))

(defpsmacro labeled-for (label init-forms cond-forms step-forms &rest body)
  (warn-deprecated 'labeled-for 'label)
  `(label ,label (for ,init-forms ,cond-forms ,step-forms ,@body)))

(defpsmacro do-set-timeout ((timeout) &body body)
  (warn-deprecated 'do-set-timeout 'set-timeout)
  `(set-timeout (lambda () ,@body) ,timeout))

(defun concat-string (&rest things)
  (warn-deprecated 'concat-string 'stringify)
  (apply #'stringify things))

(define-statement-operator with (expression &rest body)
  (warn-deprecated 'with '|LET or WITH-SLOTS|)
  `(ps-js:with ,(compile-expression expression)
     ,(compile-statement `(progn ,@body))))

(defpsmacro label (&rest args)
  (warn-deprecated 'label 'block)
  `(block ,@args))

(define-ps-symbol-macro f ps-js:false)

(setf %compiling-reserved-forms-p% nil)
