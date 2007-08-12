(in-package :parenscript)

(defun translate-ast (compiled-expr &key (output-stream *standard-output*) (output-spec :javascript) (pretty-print t))
  "Translates a compiled Parenscript program (compiled with COMPILE-PAREN-FORM)
to a Javascript string.  Outputs to the stream OUTPUT-STREAM in the language given
by OUTPUT-SPEC, pretty printing if PRETTY-PRINT is non-null.

OUTPUT-SPEC must be :javascript at the moment."
  (when (not (eql :javascript output-spec))
    (error "Unsupported output-spec for translation: ~A" output-spec))
  (write-string (string-join (ps-print compiled-expr 0)
                             (string #\Newline))
                output-stream))

(defun compile-script (script-form &key (output-spec :javascript) (pretty-print t) (output-stream nil) (toplevel-p t))
  "Compiles the Parenscript form SCRIPT-FORM into the language specified by OUTPUT-SPEC.
Non-null PRETTY-PRINT values result in a pretty-printed output code.  If OUTPUT-STREAM
is NIL, then the result is a string; otherwise code is output to the OUTPUT-STREAM stream.

This is the main function used by Parenscript users to compile their code to Javascript (and
potentially other languages)."
  (macrolet ((with-output-stream ((var) &body body)
	       `(if (null output-stream)
		 (with-output-to-string (,var)
		   ,@body)
		 (let ((,var output-stream))
		   ,@body))))
    (with-output-stream (stream)
      (translate-ast (compile-parenscript-form script-form)
                     :output-stream stream
                     :output-spec output-spec
                     :pretty-print pretty-print))))

(defun ps-to-string (expr)
  (string-join (ps-print (compile-parenscript-form expr) 0) (string #\Newline)))

(defmacro ps (&body body)
  "A macro that returns a Javascript string of the supplied Parenscript forms."
  `(ps* '(progn ,@body)))

(defun ps* (&rest body)
  "Return the javascript string representing BODY.
Body is evaluated."
  (compile-script `(progn ,@body)))
