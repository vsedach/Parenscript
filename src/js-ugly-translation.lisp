(in-package :parenscript)

(defparameter *js-lisp-escaped-chars*
  '((#\' . #\')
    (#\\ . #\\)
    (#\b . #\Backspace)
    (#\f . #.(code-char 12))
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)))

(defparameter *char-escape-table*
  (let ((hash (make-hash-table)))
    (dolist (escape-pair *js-lisp-escaped-chars* hash)
      (setf (gethash (cdr escape-pair) hash) (car escape-pair)))))

(declaim (inline lisp-special-char-to-js-2))
(defun lisp-special-char-to-js-2 (lisp-char)
  "Gets the escaped version "
  (gethash lisp-char *char-escape-table*))

(defgeneric js-translate (ast-node expression-or-statement stream)
  (:documentation "Translates the given AST node to Javascript.
expression-or-statement is either the keyword :statement or :expression"))

(defmacro defjstrans (script-class type-spec (node-var stream-var) &body body)
  "Generates a translate-to-js definition for the special-form class SCRIPT-CLASS
where type-spec is either :expression or :statement.  STREAM is the output stream
where we should place the Javascript."
  (when (not (or (eql :expression type-spec) (eql :statement type-spec)))
    (error "Invalid type-spec fo DEFJSTRANS form."))
  `(defmethod js-translate ((,node-var ,script-class) (spec (eql ,type-spec)) ,stream-var)
    ,@body))

(defjstrans expression :expression (expr stream)
  (princ (value expr) stream))

(defjstrans expression :statement (expr stream)
  (princ (value expr) stream))

(defjstrans statement :statement (statement stream)
  (princ (value statement) stream))

(defmacro dolist+ ((car-var list &key result-form lastp-var) &body body)
  "Iterates over a list, giving other information in bindings designated
by the keyword arguments."
  (let ((sublist-var (gensym)))
    `(progn
      (mapl
       #'(lambda (,sublist-var)
	   (let ((,car-var (car ,sublist-var))
		 ,@(when lastp-var 
			 (list `(,lastp-var (not (cdr ,sublist-var))))))
	     ,@body))
       ,list)
      ,result-form)))
	    
	    
(defjstrans array-literal :expression (array stream)
  (write-char #\[ stream)
  (dolist+ (array-item (array-values array) :lastp-var last?)
    (js-translate array-item :expression stream)
    (when (not last?) (princ ",")))
  (write-char #\] stream))

(defjstrans script-aref :expression (aref stream)
  (js-translate (aref-array aref) :expression stream)
  (princ "[")
  (js-translate (aref-index aref) :expression stream)
  (princ "]"))

(defjstrans object-literal :expression (obj stream)
  (princ "{")
  (dolist+ (obj-pair (object-values obj) :lastp-var last?)
    (js-translate (car obj-pair) :expression stream)
    (princ ":")
    (js-translate (cdr obj-pair) :expression stream)
    (when (not last?) (princ ",")))
  (princ "}"))

(defjstrans string-literal :expression (string stream)
  (declare (inline lisp-special-char-to-js-2))
  (write-char *js-quote-char*  stream)
  (loop
   for char across (value string)
   for code = (char-code char)
   for special = (lisp-special-char-to-js-2 char)
   do
   (cond
     (special
      (write-char #\\ stream)
      (write-char special stream))
     ((or (<= code #x1f) (>= code #x80))
      (format stream "\\u~4,'0x" code))
     (t (write-char char stream)))
   finally (write-char *js-quote-char* stream)))

(defjstrans script-variable :expression (var stream)
  (princ (symbol-to-js (value var)) stream))

(defjstrans op-form :expression (op-form stream)
  (let ((precedence (expression-precedence op-form)))
    (flet ((output-op-arg (op-arg)
	     (let ((parens? (>= (expression-precedence op-arg) precedence)))
	       (when parens? (write-char #\())
	       (js-translate op-arg :expression stream)
	       (when parens? (write-char #\))))))
      (output-op-arg (first (op-args op-form)))
      (format stream "~A " (operator op-form))
      (output-op-arg (second (op-args op-form))))))

(defjstrans one-op :expression (one-op stream)
  (let ((pre? (one-op-pre-p one-op)))
    (when pre?
      (princ (one-op one-op) stream))
    (js-translate (value one-op) :expression stream)
    (when (not pre?)
      (princ (one-op one-op) stream))))