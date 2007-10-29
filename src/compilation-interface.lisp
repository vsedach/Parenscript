(in-package :parenscript)

(defun compile-script (script-form &key (output-stream nil))
  "Compiles the Parenscript form SCRIPT-FORM into Javascript.
Non-null PRETTY-PRINT values result in a pretty-printed output code.
If OUTPUT-STREAM is NIL, then the result is a string; otherwise code
is output to the OUTPUT-STREAM stream."
  (parenscript-print (compile-parenscript-form script-form :expecting :statement) output-stream))

(defmacro ps (&body body)
  "Given Parenscript forms (an implicit progn), expands to code which
compiles those forms to a JavaScript string."
  `(ps* '(progn ,@body)))

(defun ps* (&rest body)
  "Compiles BODY to a JavaScript string.
Body is evaluated."
  (compile-script `(progn ,@body)))

(defvar *js-inline-string-delimiter* #\"
  "Controls the string delimiter char used when compiling Parenscript in ps-inline.")

(defun ps-inline* (form &optional (*js-string-delimiter* *js-inline-string-delimiter*))
  (concatenate 'string
               "javascript:"
               (parenscript-print (compile-parenscript-form form :expecting :statement))))

(defmacro ps-inline (form &optional (string-delimiter '*js-inline-string-delimiter*))
  `(ps-inline* ',form ,string-delimiter))
