(in-package :parenscript)

(defmacro with-new-compilation-environment ((var) &body body)
  `(let* ((,var (make-basic-compilation-environment))
	  (*compilation-environment* ,var))
    ,@body))
    
(defun translate-ast (compiled-expr
		      &key
		      (comp-env *compilation-environment*)
		      (output-stream *standard-output*)
		      (output-spec :javascript)
		      (pretty-print t))
  "Translates a compiled Parenscript program (compiled with COMPILE-PAREN-FORM)
to a Javascript string.  Outputs to the stream OUTPUT-STREAM in the language given
by OUTPUT-SPEC, pretty printing if PRETTY-PRINT is non-null.

OUTPUT-SPEC must be :javascript at the moment."
  (declare (ignore comp-env))
  (when (not (eql :javascript output-spec))
    (error "Unsupported output-spec for translation: ~A" output-spec))
  (when (eql :javascript output-spec)
;    (if (not pretty-print)
;	(js-translate compiled-expr :statement output-stream)
	(write-string (string-join
		       (ps-js::js-to-statement-strings compiled-expr 0)
		       (string #\Newline))
		      output-stream)))

(defun non-nil-comp-env ()
  "Returns a sane compilation environment.  Either the one currently bound or a new
one."
  (or *compilation-environment*
      (make-basic-compilation-environment)))


(defun compile-script (script-form
		       &key
		       (output-spec :javascript)
		       (pretty-print t)
		       (output-stream nil)
		       (toplevel-p t)
		       (comp-env (non-nil-comp-env)))
  "Compiles the Parenscript form SCRIPT-FORM into the language specified by OUTPUT-SPEC.
Non-null PRETTY-PRINT values result in a pretty-printed output code.  If OUTPUT-STREAM
is NIL, then the result is a string; otherwise code is output to the OUTPUT-STREAM stream.
COMP-ENV is the compilation environment in which to compile the form.

This is the main function used by Parenscript users to compile their code to Javascript (and
potentially other languages)."
  (macrolet ((with-output-stream ((var) &body body)
	       `(if (null output-stream)
		 (with-output-to-string (,var)
		   ,@body)
		 (let ((,var output-stream))
		   ,@body))))
    ;; we might want to bind this rather than set it
    (setf (comp-env-compiling-toplevel-p comp-env) toplevel-p)
    (with-output-stream (stream)
      (let* ((*compilation-environment* comp-env)
	     (compiled (let ((first-result (compile-parenscript-form comp-env script-form)))
                         (if (not toplevel-p)
                             first-result
                             (progn
                               (setf (comp-env-compiling-toplevel-p comp-env) nil)
                               (compile-parenscript-form comp-env first-result))))))
	(translate-ast
	 compiled
;	 (compile-script-form script-form :comp-env comp-env)
	 :comp-env comp-env
	 :output-stream stream
	 :output-spec output-spec
	 :pretty-print pretty-print)))))

(defun ps-to-string (expr)
  "Given an AST node, compiles it to a Javascript string."
  (string-join
   (ps-js::js-to-statement-strings (compile-script-form expr) 0)
   (string #\Newline)))

;;; SEXPs -> Javascript string functionality
(defmacro script (&body body)
  "A macro that returns a Javascript string of the supplied Parenscript forms."
  `(script* '(progn ,@body)))

(defun script* (&rest body)
  "Return the javascript string representing BODY.
Body is evaluated."
  (compile-script `(progn ,@body)))

;;; Handy synonyms
(defmacro ps (&body body)
  `(script ,@body))

(defmacro ps* (&body body)
  `(script* ,@body))

(defmacro js (&body body)
  `(script ,@body))

(defmacro js* (&body body)
  `(script* ,@body))
