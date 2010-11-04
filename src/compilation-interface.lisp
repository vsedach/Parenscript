(in-package #:parenscript)

(defparameter *js-target-version* 1.3)

(defvar *parenscript-stream* nil)

(defmacro ps (&body body)
  "Given Parenscript forms (an implicit progn), compiles those forms
to a JavaScript string at macro-expansion time."
  (let ((printed-forms (parenscript-print
                        (compile-statement `(progn ,@body))
                        nil)))
    (if (and (not (cdr printed-forms))
             (stringp (car printed-forms)))
        (car printed-forms)
        (let ((s (gensym)))
          `(with-output-to-string (,s)
             ,@(mapcar (lambda (x) `(write-string ,x ,s)) printed-forms))))))

(defmacro ps-to-stream (stream &body body)
  (let ((printed-forms (parenscript-print
                        (compile-statement `(progn ,@body))
                        nil)))
    `(let ((*parenscript-stream* ,stream))
       ,@(mapcar (lambda (x) `(write-string ,x *parenscript-stream*)) printed-forms))))

(defun ps* (&rest body)
  "Compiles BODY to a JavaScript string.
Body is evaluated."
  (let ((*psw-stream* (or *parenscript-stream* (make-string-output-stream))))
    (parenscript-print (compile-statement `(progn ,@body)) t)
    (unless *parenscript-stream*
      (get-output-stream-string *psw-stream*))))

(defmacro ps-doc (&body body)
  "Expands Parenscript forms in a clean environment."
  (let ((*ps-gensym-counter* 0)
        (*special-variables* nil))
     (macroexpand-1 `(ps ,@body))))

(defun ps-doc* (&rest body)
  (let ((*ps-gensym-counter* 0)
        (*special-variables* nil))
    (apply #'ps* body)))

(defvar *js-inline-string-delimiter* #\"
  "Controls the string delimiter char used when compiling Parenscript in ps-inline.")

(defun ps-inline* (form &optional (*js-string-delimiter* *js-inline-string-delimiter*))
  (concatenate 'string "javascript:" (ps* form)))

(defmacro+ps ps-inline (form &optional (string-delimiter *js-inline-string-delimiter*))
  `(concatenate 'string "javascript:"
                ,@(let ((*js-string-delimiter* string-delimiter))
                    (parenscript-print (compile-statement form) nil))))

(defvar *ps-read-function* #'read)

(defun ps-compile-stream (stream)
  (let ((*compilation-level* :toplevel)
	(eof '#:eof))
    (ps* (cons 'progn
               (loop for form = (funcall *ps-read-function* stream nil eof)
                     until (eq form eof)
                     collect form)))))

(defun ps-compile-file (source-file)
  (with-open-file (stream source-file :direction :input)
    (ps-compile-stream stream)))
