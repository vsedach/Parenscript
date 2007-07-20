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
  (declare (ignore pretty-print) (ignore comp-env))
  (when (not (eql :javascript output-spec))
    (error "Unsupported output-spec for translation: ~A" output-spec))
  (when (eql :javascript output-spec)
    (write-string (string-join
		   (js-to-statement-strings compiled-expr 0)
		   (string #\Newline))
		  output-stream)))

(defun compile-script (script-form
		       &key
		       (output-spec :javascript)
		       (pretty-print t)
		       (output-stream nil)
		       (toplevel-p t)
		       (comp-env (make-basic-compilation-environment)))
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
    (with-output-stream (stream)
      (let* ((*compilation-environment* comp-env)
	     (compiled
	      (if toplevel-p
		  (compile-parenscript-form 
		   comp-env
		   (compile-parenscript-form comp-env script-form :toplevel-p t))
		  (compile-parenscript-form comp-env script-form :toplevel-p nil))))
	(translate-ast
	 compiled
;	 (compile-script-form script-form :comp-env comp-env)
	 :comp-env comp-env
	 :output-stream stream
	 :output-spec output-spec
	 :pretty-print pretty-print)))))

(defun compile-script-file (source-file
			    &key
			    (output-spec :javascript)
			    (comp-env (or *compilation-environment*
					  (make-basic-compilation-environment)))
			    (pretty-print t)
			    (output-stream *standard-output*))
  "Compiles the given Parenscript source file and outputs the results
to the given output stream."
  (setf (comp-env-compiling-toplevel-p comp-env) t)
  (error "NOT IMPLEMENTED."))
	
	


;(defun compile-script-file (script-src-file
;			    &key
;			    (output-spec :javascript)
;			    (output-stream *standard-out*)
;			    (comp-env *compilation-environment*))
			    

;;; SEXPs -> Javascript string functionality
(defmacro script (&body body)
  "A macro that returns a Javascript string of the supplied Parenscript forms."
  `(js* '(progn ,@body)))

(defmacro script* (&body body)
  "Return the javascript string representing BODY.

Body is evaluated."
  `(compile-script (progn ,@body)))

;; DEPRECATED
(defmacro js (&body body)
  "A macro that returns a javascript string of the supplied Parenscript forms."
  `(script ,@body))

(defmacro js* (&body body)
  `(script* ,@body))

(defun js-to-string (expr)
  "Given an AST node, compiles it to a Javascript string."
  (string-join
   (js-to-statement-strings (compile-script-form expr) 0)
   (string #\Newline)))

(defun js-to-line (expr)
  "Given an AST node, compiles it to a Javascript string."
  (string-join
   (js-to-statement-strings (compile-script-form expr) 0) " "))


;;; old file compilation functions:
(defun compile-parenscript-file-to-string (source-file
					   &key
					   (log-stream nil)
					   (comment nil)
					   (eval-forms-p nil))
  "Compile SOURCE-FILE (a parenscript file) to a javascript string. (in-package ...) forms
behave as expected and all other forms are evaluated according to the value of
EVAL-FORMS-P. If the result of the evaluation is not nil then it's compiled with
js:js* and written to the output."
  (with-output-to-string (output)
    (with-open-file (input source-file :direction :input)
      (flet ((read-form ()
               (read input nil))
             (log-message (&rest args)
               (when log-stream
                 (apply #'format log-stream args))))
        (let ((*package* *package*))
          (loop for form = (read-form)
                while form do
                (if (or (not (listp form))
                        (not (eq (car form) 'cl:in-package)))
                    (progn
                      (log-message "Processing form:~%~S~%" form)
                      (when comment
                        (princ "/*" output)
                        (print form output)
                        (terpri output)
                        (princ "*/" output)
                        (terpri output))
                      (when eval-forms-p
                        (setf form (eval form)))
                      (log-message "After evaluation:~%~S~%" form)
                      (when form
                        (let ((compiled (js:js* form)))
                          (log-message "Compiled into:~%~A~%~%" compiled)
                          (write-string compiled output)
                          (terpri output)
                          (terpri output))))
                    (when (and (listp form)
                               (eq (car form) 'cl:in-package))
                      (log-message "Setting package to: ~S~%" (cadr form))
                      (setf *package* (find-package (cadr form)))))))))))

(defun compile-parenscript-file (source-file &rest args &key destination-file &allow-other-keys)
  "Compile SOURCE-FILE (a parenscript file) to a javascript file with
compile-parenscript-file-to-string. When DESTINATION-FILE is omitted,
then it will be named the same as SOURCE-FILE but with js extension."
  (setf args (copy-list args))
  (remf args :destination-file)
  (unless destination-file
    (setf destination-file (merge-pathnames (make-pathname :type "js")
                                            source-file)))
  (with-open-file (output destination-file :if-exists :supersede :direction :output)
    (write-string (apply #'compile-parenscript-file-to-string source-file args) output)))
