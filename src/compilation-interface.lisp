(in-package :parenscript)

(defmacro ps (&body body)
  "Given Parenscript forms (an implicit progn), compiles those forms
to a JavaScript string at macro-expansion time."
  `(concatenate 'string ,@(parenscript-print (compile-parenscript-form `(progn ,@body) :expecting :statement))))

(defmacro ps-doc (&body body)
  "Expands Parenscript forms in a clean environment."
  (let ((*ps-gensym-counter* 0)
        (*ps-special-variables* nil))
     (macroexpand-1 `(ps ,@body))))

(defun ps1* (ps-form)
  (apply #'concatenate 'string
         (mapcar (lambda (x)
                   (if (stringp x)
                       x
                       (eval x)))
                 (parenscript-print (compile-parenscript-form ps-form :expecting :statement)))))

(defun ps* (&rest body)
  "Compiles BODY to a JavaScript string.
Body is evaluated."
  (ps1* `(progn ,@body)))

(defvar *js-inline-string-delimiter* #\"
  "Controls the string delimiter char used when compiling Parenscript in ps-inline.")

(defun ps-inline* (form &optional (*js-string-delimiter* *js-inline-string-delimiter*))
  (concatenate 'string "javascript:" (ps1* form)))

(defmacro/ps ps-inline (form &optional (string-delimiter *js-inline-string-delimiter*))
  `(concatenate 'string "javascript:"
                ,@(let ((*js-string-delimiter* string-delimiter))
                    (parenscript-print (compile-parenscript-form form :expecting :statement)))))
