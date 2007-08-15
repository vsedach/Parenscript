(in-package :parenscript)

(defun compile-script (script-form &key (output-stream nil))
  "Compiles the Parenscript form SCRIPT-FORM into Javascript.
Non-null PRETTY-PRINT values result in a pretty-printed output code.
If OUTPUT-STREAM is NIL, then the result is a string; otherwise code
is output to the OUTPUT-STREAM stream."
  (parenscript-print (compile-parenscript-form script-form) output-stream))

(defmacro ps (&body body)
  "A macro that returns a Javascript string of the supplied Parenscript forms."
  `(ps* '(progn ,@body)))

(defun ps* (&rest body)
  "Return the javascript string representing BODY.
Body is evaluated."
  (compile-script `(progn ,@body)))
