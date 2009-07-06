(in-package "PARENSCRIPT")

(defparameter *js-target-version* 1.3)

(defmacro ps (&body body)
  "Given Parenscript forms (an implicit progn), compiles those forms
to a JavaScript string at macro-expansion time."
  (let ((s (gensym)))
    `(with-output-to-string (,s)
       ,@(mapcar (lambda (x)
                   `(write-string ,x ,s))
                 (parenscript-print
                  (compile-parenscript-form `(progn ,@body)
                                            :expecting :statement))))))
(defun ps* (&rest body)
  "Compiles BODY to a JavaScript string.
Body is evaluated."
  (compiled-form-to-string (compile-parenscript-form `(progn ,@body) :expecting :statement)))

(defmacro ps-doc (&body body)
  "Expands Parenscript forms in a clean environment."
  (let ((*ps-gensym-counter* 0)
        (*ps-special-variables* nil))
     (macroexpand-1 `(ps ,@body))))

(defun ps-doc* (ps-form)
  (let ((*ps-gensym-counter* 0)
        (*ps-special-variables* nil))
    (ps* ps-form)))

(defun compiled-form-to-string (ps-compiled-form)
  (with-output-to-string (s)
    (dolist (x (parenscript-print ps-compiled-form))
      (write-string (if (stringp x) x (eval x)) s))))

(defvar *js-inline-string-delimiter* #\"
  "Controls the string delimiter char used when compiling Parenscript in ps-inline.")

(defun ps-inline* (form &optional (*js-string-delimiter* *js-inline-string-delimiter*))
  (concatenate 'string "javascript:" (ps* form)))

(defmacro/ps ps-inline (form &optional (string-delimiter *js-inline-string-delimiter*))
  `(concatenate 'string "javascript:"
                ,@(let ((*js-string-delimiter* string-delimiter))
                    (parenscript-print (compile-parenscript-form form :expecting :statement)))))

(defvar *ps-read-function* #'read
  "This should be a function that takes the same inputs and returns the same
outputs as the common lisp read function.  We declare it as a variable to allow
a user-supplied reader instead of the default lisp reader.")

(defun ps-compile-stream (stream)
  "Compiles a source stream as if it were a file.  Outputs a Javascript string."
  (let ((*ps-compilation-level* :toplevel)
	(*package* *package*)
	(end-read-form '#:unique))
    (flet ((read-form () (funcall *ps-read-function* stream nil end-read-form)))
      (let* ((js-string
	      ;; cons up the forms, compiling as we go, and print the result
	      (do ((form (read-form) (read-form))
		   (compiled-forms nil))
		  ((eql form end-read-form)
		     (format nil "~{~A~^;~%~}"
			     (remove-if
			      #'(lambda (x) (or (null x) (= 0 (length x))))
			      (mapcar 'compiled-form-to-string (nreverse compiled-forms)))))
		(push (compile-parenscript-form form :expecting :statement) compiled-forms))))
	js-string))))

(defun ps-compile-file (source-file)
  "Compiles the given Parenscript source file and returns a Javascript string."
  (with-open-file (stream source-file :direction :input)
    (ps-compile-stream stream)))
