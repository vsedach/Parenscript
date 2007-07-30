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
			    (comp-env (non-nil-comp-env))
			    (pretty-print t)
			    (output-stream *standard-output*))
  "Compiles the given Parenscript source file and outputs the results
to the given output stream."
  (setf (comp-env-compiling-toplevel-p comp-env) t)
  (with-open-file (input source-file :direction :input)
    (let ((end-read-form '#:unique))
      (flet ((read-form ()
	       (parenscript.reader:read input nil end-read-form)))
	(macrolet ((with-output-stream ((var) &body body)
		     `(if (null output-stream)
		       (with-output-to-string (,var)
			 ,@body)
		       (let ((,var output-stream))
			 ,@body))))
	  (let* ((*compilation-environment* comp-env)
		 (compiled
		  (do ((form (read-form) (read-form))
		       (compiled-forms nil))
		      ((eql form end-read-form)
		       (compile-parenscript-form 
			comp-env
			`(progn ,@(nreverse compiled-forms))
			:toplevel-p nil))
		    (let ((tl-compiled-form
			   (compile-parenscript-form comp-env form :toplevel-p t)))
		      (push tl-compiled-form compiled-forms)))))
	    (with-output-stream (output)
	      (translate-ast
	       compiled
	       :comp-env comp-env
	       :output-stream output
	       :output-spec output-spec
	       :pretty-print pretty-print))))))))

(defun compile-script-system (system 
			      &rest args
			      &key
			      (output-spec :javascript)
			      (pretty-print t)
			      (output-to-stream t)
			      (output-stream *standard-output*)
			      output-to-files ;; currently ignored
			      (comp-env (non-nil-comp-env)))
  "Compiles a collection of parenscripts as described by an ASDF system into files or
a specified output stream."
  (asdf:operate 'asdf::parenscript-compile-op system
		:output-spec output-spec
		:pretty-print pretty-print
;		:output-to-stream t
		:output-stream output-stream
		:comp-env comp-env
		:force-p t
		))
	 

;(defun compile-script-system-component (system-designator 

;(defun compile-script-file (script-src-file
;			    &key
;			    (output-spec :javascript)
;			    (output-stream *standard-out*)
;			    (comp-env *compilation-environment*))
			    

;;; old file compilation functions:
(defun compile-parenscript-file-to-string (source-file)
  "Compile SOURCE-FILE (a parenscript file) to a javascript string. (in-package ...) forms
behave as expected and all other forms are evaluated according to the value of
EVAL-FORMS-P. If the result of the evaluation is not nil then it's compiled with
js:js* and written to the output."
  (compile-script-file source-file :output-stream nil))
  
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
