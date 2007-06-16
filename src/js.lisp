(in-package :js)

;;; ecmascript standard:
;;; http://www.ecma-international.org/publications/standards/Ecma-262.htm

;;; javascript name conversion

(defparameter *special-chars*
  '((#\! . "Bang")
    (#\? . "What")
    (#\# . "Hash")
    (#\@ . "At")
    (#\% . "Percent")
    (#\+ . "Plus")
    (#\* . "Star")
    (#\/ . "Slash")))

(defun string-chars (string)
  (coerce string 'list))

(defun constant-string-p (string)
  (let ((len (length string))
        (constant-chars '(#\+ #\*)))
    (and (> len 2)
         (member (char string 0) constant-chars)
         (member (char string (1- len)) constant-chars))))

(defun first-uppercase-p (string)
  (and (> (length string) 1)
       (member (char string 0) '(#\+ #\*))))

(defun untouchable-string-p (string)
  (and (> (length string) 1)
       (char= #\: (char string 0))))

(defun symbol-to-js (symbol)
  (when (symbolp symbol)
    (setf symbol (symbol-name symbol)))
  (let ((symbols (string-split symbol '(#\.))))
    (cond ((null symbols) "")
	  ((= (length symbols) 1)
	   (let (res
                 (do-not-touch nil)
		 (lowercase t)
		 (all-uppercase nil))
	     (cond ((constant-string-p symbol)
		    (setf all-uppercase t
			  symbol (subseq symbol 1 (1- (length symbol)))))
		   ((first-uppercase-p symbol)
		    (setf lowercase nil
			  symbol (subseq symbol 1)))
                   ((untouchable-string-p symbol)
                    (setf do-not-touch t
                          symbol (subseq symbol 1))))
	     (flet ((reschar (c)
		      (push (cond
                              (do-not-touch c)
                              ((and lowercase (not all-uppercase))
                               (char-downcase c))
                              (t (char-upcase c)))
                            res)
		      (setf lowercase t)))
	       (dotimes (i (length symbol))
		 (let ((c (char symbol i)))
		   (cond
		     ((eql c #\-)
		      (setf lowercase (not lowercase)))
		     ((assoc c *special-chars*)
		      (dolist (i (coerce (cdr (assoc c *special-chars*)) 'list))
			(reschar i)))
		     (t (reschar c))))))
	     (coerce (nreverse res) 'string)))
	  (t (string-join (mapcar #'symbol-to-js symbols) ".")))))

;;; js language types

(defmethod js-equal ((obj1 list) (obj2 list))
  (and (= (length obj1) (length obj2))
       (every #'js-equal obj1 obj2)))
(defmethod js-equal ((obj1 t) (obj2 t))
  (equal obj1 obj2))

(defmacro defjsclass (name superclasses slots &rest class-options)
  (let ((slot-names (mapcar #'(lambda (slot) (if (atom slot) slot (first slot))) slots)))
    `(progn
      (defclass ,name ,superclasses
	,slots ,@class-options)
      (defmethod js-equal ((obj1 ,name) (obj2 ,name))
	(every #'(lambda (slot)
		   (js-equal (slot-value obj1 slot)
			     (slot-value obj2 slot)))
	       ',slot-names)))))

(defclass statement ()
  ((value :initarg :value :accessor value :initform nil)))

(defclass expression (statement)
  ((value)))

;;; indenter

(defun special-append-to-last (form elt)
  (flet ((special-append (form elt)
	   (let ((len (length form)))
	     (if (and (> len 0)
                      (string= (char form (1- len)) elt))
		 form
		 (concatenate 'string form elt)))))
    (cond ((stringp form)
	   (special-append form elt))
	  ((consp form)
	   (let ((last (last form)))
	     (if (stringp (car last))
		 (rplaca last (special-append (car last) elt))
		 (append-to-last (car last) elt))
	   form))
	  (t (error "unsupported form ~S" form)))))

(defun dwim-join (value-string-lists max-length
		  &key (start "")
                       end
		       (join-before "")
                       join-after
		       (white-space (make-string (length start) :initial-element #\Space))
                       (separator " ")
		  (append-to-last #'append-to-last)
		  (collect t))
    #+nil
    (format t "value-string-lists: ~S~%" value-string-lists)

    ;;; collect single value-string-lists until line full

    (do* ((string-lists value-string-lists (cdr string-lists))
	  (string-list (car string-lists) (car string-lists))
	  (cur-elt start)
          (is-first t nil)
	  (cur-empty t)
	  (res nil))
	 ((null string-lists)
	  (unless cur-empty
	    (push cur-elt res))
	  (if (null res)
	      (list (concatenate 'string start end))
	      (progn
		(when end
		  (setf (first res)
			(funcall append-to-last (first res) end)))
		(nreverse res))))
      #+nil
      (format t "string-list: ~S~%" string-list)

      (when join-after
	(unless (null (cdr string-lists))
	  (funcall append-to-last string-list join-after)))

      (if (and collect (= (length string-list) 1))
	  (progn
	    #+nil
	    (format t "cur-elt: ~S line-length ~D, max-length ~D, string: ~S~%"
		    cur-elt
		    (+ (length (first string-list))
		       (length cur-elt))
		    max-length
		    (first string-list))
	    (if (or cur-empty
		    (< (+ (length (first string-list))
			  (length cur-elt)) max-length))
		(setf cur-elt
		      (concatenate 'string cur-elt
				   (if (or is-first (and cur-empty (string= join-before "")))
                                        "" (concatenate 'string separator join-before))
				   (first string-list))
		      cur-empty nil)
		(progn
		  (push cur-elt res)
		  (setf cur-elt (concatenate 'string white-space
					     join-before (first string-list))
			cur-empty nil))))

	  (progn
	    (unless cur-empty
	      (push cur-elt res)
	      (setf cur-elt white-space
		    cur-empty t))
	    (setf res (nconc (nreverse
			      (cons (concatenate 'string
						 cur-elt
                                                 (if (null res)
						     "" join-before)
						 (first string-list))
				    (mapcar #'(lambda (x) (concatenate 'string white-space x))
					    (cdr string-list))))
                             res))
	    (setf cur-elt white-space cur-empty t)))))

(defmethod js-to-strings ((expression expression) start-pos)
  (declare (ignore start-pos))
  (list (princ-to-string (value expression))))

(defmethod js-to-statement-strings ((expression expression) start-pos)
  (js-to-strings expression start-pos))

(defmethod js-to-statement-strings ((statement statement) start-pos)
  (declare (ignore start-pos))
  (list (princ-to-string (value statement))))

;;; compiler macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *js-compiler-macros* (make-hash-table :test 'equal)
        "*JS-COMPILER-MACROS* is a hash-table containing the functions corresponding
to javascript special forms, indexed by their name. Javascript special
forms are compiler macros for JS expressions.")

  (defun undefine-js-compiler-macro (name)
    (declare (type symbol name))
    (when (gethash (symbol-name name) *js-compiler-macros*)
      (warn "Redefining js compiler macro ~S" name)
      (remhash (symbol-name name) *js-compiler-macros*))))

(defmacro define-js-compiler-macro (name lambda-list &rest body)
  "Define a javascript compiler macro NAME. Arguments are destructured
according to LAMBDA-LIST. The resulting JS language types are appended
to the ongoing javascript compilation."
  (let ((js-name (intern (concatenate 'string "JS-" (symbol-name name)) #.*package*)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defun ,js-name ,lambda-list ,@body)
      (setf (gethash ,(symbol-name name) *js-compiler-macros*) #',js-name))))

(defun js-compiler-macro-form-p (form)
  (when (and (symbolp (car form))
             (gethash (symbol-name (car form)) *js-compiler-macros*))
    t))

(defun js-get-compiler-macro (name)
  (when (symbolp name)
    (gethash (symbol-name name) *js-compiler-macros*)))

;;; macro expansion

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *js-macro-toplevel* (make-hash-table :test 'equal)
    "Toplevel of macro expansion, holds all the toplevel javascript macros.")
  (defvar *js-macro-env* (list *js-macro-toplevel*)
    "Current macro environment."))

(defun lookup-macro (name)
  "Lookup the macro NAME in the current macro expansion
environment. Returns the macro and the parent macro environment of
this macro."
  (unless (symbolp name)
    (return-from lookup-macro nil))
  (do ((env *js-macro-env* (cdr env)))
      ((null env) nil)
    (let ((val (gethash (symbol-name name) (car env))))
      (when val
	(return-from lookup-macro
	  (values val (or (cdr env)
			  (list *js-macro-toplevel*))))))))

(defmacro defjsmacro (name args &rest body)
  "Define a javascript macro, and store it in the toplevel macro environment."
  (let ((lambda-list (gensym)))
    (undefine-js-compiler-macro name)
    `(setf (gethash ,(symbol-name name) *js-macro-toplevel*)
      #'(lambda (&rest ,lambda-list)
          (destructuring-bind ,args ,lambda-list ,@body)))))

(defun import-macros-from-lisp (&rest names)
  "Import the named lisp macros into the js macro expansion"
  (dolist (name names)
    (let ((name name))
      (undefine-js-compiler-macro name)
      (setf (gethash (symbol-name name) *js-macro-toplevel*)
            (lambda (&rest args)
              (macroexpand `(,name ,@args)))))))

(defun js-expand-form (expr)
  "Expand a javascript form."
  (cond ((atom expr)
	 (multiple-value-bind (js-macro macro-env)
	     (lookup-macro expr)
	   (if js-macro
	       (js-expand-form (let ((*js-macro-env* macro-env))
				 (funcall js-macro)))
	       expr)))

	((js-compiler-macro-form-p expr) expr)

	((equal (first expr) 'quote) expr)

	(t (let ((js-macro (lookup-macro (car expr))))
	     (if js-macro
		 (js-expand-form (apply js-macro (cdr expr)))
		 expr)))))

(defvar *gen-js-name-counter* 0)

(defun gen-js-name-string (&key (prefix "_ps_"))
  "Generates a unique valid javascript identifier ()"
  (concatenate 'string
               prefix (princ-to-string (incf *gen-js-name-counter*))))

(defun gen-js-name (&key (prefix "_ps_"))
  "Generate a new javascript identifier."
  (intern (gen-js-name-string :prefix prefix)
          (find-package :js)))

(defmacro with-unique-js-names (symbols &body body)
  "Evaluate BODY with the variables on SYMBOLS bound to new javascript identifiers.

Each element of SYMBOLS is either a symbol or a list of (symbol
prefix)."
  `(let* ,(mapcar (lambda (symbol)
                    (destructuring-bind (symbol &optional prefix)
                        (if (consp symbol)
                            symbol
                            (list symbol))
                      (if prefix
                          `(,symbol (gen-js-name :prefix ,prefix))
                          `(,symbol (gen-js-name)))))
                  symbols)
     ,@body))

(defjsmacro rebind (variables expression)
  ;; Creates a new js lexical environment and copies the given
  ;; variable(s) there.  Executes the body in the new environment. This
  ;; has the same effect as a new (let () ...) form in lisp but works on
  ;; the js side for js closures."
  
  (unless (listp variables)
    (setf variables (list variables)))
  `((lambda ()
      (let ((new-context (new *object)))
        ,@(loop for variable in variables
                do (setf variable (symbol-to-js variable))
                collect `(setf (slot-value new-context ,variable) (slot-value this ,variable)))
        (with new-context
              (return ,expression))))))

(defvar *var-counter* 0)

(defun js-gensym (&optional (name "js"))
  (intern (format nil "tmp-~A-~A" name (incf *var-counter*)) #.*package*))

;;; literals

(defmacro defjsliteral (name string)
  "Define a Javascript literal that will expand to STRING."
  `(define-js-compiler-macro ,name () (make-instance 'expression :value ,string)))

(defjsliteral this      "this")
(defjsliteral t         "true")
(defjsliteral nil       "null")
(defjsliteral false     "false")
(defjsliteral undefined "undefined")

(defmacro defjskeyword (name string)
  "Define a Javascript keyword that will expand to STRING."
  `(define-js-compiler-macro ,name () (make-instance 'statement :value ,string)))

(defjskeyword break    "break")
(defjskeyword continue "continue")

;;; array literals

(defjsclass array-literal (expression)
  ((values :initarg :values :accessor array-values)))

(define-js-compiler-macro array (&rest values)
  (make-instance 'array-literal
		 :values (mapcar #'js-compile-to-expression values)))

(defjsmacro list (&rest values)
  `(array ,@values))

(defmethod js-to-strings ((array array-literal) start-pos)
  (let ((value-string-lists
	 (mapcar #'(lambda (x) (js-to-strings x (+ start-pos 2)))
		 (array-values array)))
	(max-length (- 80 start-pos 2)))
    (dwim-join value-string-lists max-length
	       :start "[ " :end " ]"
	       :join-after ",")))

(defjsclass js-aref (expression)
  ((array :initarg :array
	  :accessor aref-array)
   (index :initarg :index
	  :accessor aref-index)))

(define-js-compiler-macro aref (array &rest coords)
  (make-instance 'js-aref
		 :array (js-compile-to-expression array)
		 :index (mapcar #'js-compile-to-expression coords)))

(defmethod js-to-strings ((aref js-aref) start-pos)
  (dwim-join (cons (js-to-strings (aref-array aref) start-pos)
		   (mapcar #'(lambda (x) (dwim-join (list (js-to-strings x (+ start-pos 2)))
						    (- 80 start-pos 2)
						    :start "[" :end "]"))
			   (aref-index aref)))
	     (- 80 start-pos 2) :separator ""
	     :white-space "  "))

(defjsmacro make-array (&rest inits)
  `(new (*array ,@inits)))

;;; object literals (maps and hash-tables)

(defjsclass object-literal (expression)
  ((values :initarg :values :accessor object-values)))

(define-js-compiler-macro {} (&rest values)
  (make-instance 'object-literal
                 :values (loop
                            for (key value) on values by #'cddr
                            collect (cons key (js-compile-to-expression value)))))

(defmethod js-to-strings ((obj object-literal) start-pos)
  (dwim-join (loop
                for (key . value) in (object-values obj)
                append (list
                        (dwim-join (list (list (format nil "~A:" (symbol-to-js key)))
                                         (js-to-strings value (+ start-pos 2)))
                                   (- 80 start-pos 2)
                                   :start "" :end "" :join-after "")))
             (- 80 start-pos 2)
             :start "{ " :end " }"
             :join-after ","))

;;; string literals

(defjsclass string-literal (expression)
  (value))

(defvar *js-quote-char* #\'
  "Specifies which character JS sholud use for delimiting strings.

This variable is usefull when have to embed some javascript code
in an html attribute delimited by #\\\" as opposed to #\\', or
vice-versa.")

(defmethod js-to-strings ((string string-literal) start-pos)
  (declare (ignore start-pos)
           (inline lisp-special-char-to-js))
  (list (with-output-to-string (escaped)
          (write-char *js-quote-char*  escaped)
          (loop
           for char across (value string)
           for code = (char-code char)
           for special = (lisp-special-char-to-js char)
           do
           (cond
             (special
              (write-char #\\ escaped)
              (write-char special escaped))
             ((or (<= code #x1f) (>= code #x80))
              (format escaped "\\u~4,'0x" code))
             (t (write-char char escaped)))
           finally (write-char *js-quote-char* escaped)))))

(defparameter *js-lisp-escaped-chars*
  '((#\' . #\')
    (#\\ . #\\)
    (#\b . #\Backspace)
    (#\f . #.(code-char 12))
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)))

(defun lisp-special-char-to-js(lisp-char)
    (car (rassoc lisp-char *js-lisp-escaped-chars*)))

;;; number literals

(defjsclass number-literal (expression)
  (value))

;;; variables

(defjsclass js-variable (expression)
  (value))

(defmethod js-to-strings ((v js-variable) start-form)
  (declare (ignore start-form))
  (list (symbol-to-js (value v))))

;;; quote

(defjsclass js-quote (expression)
  ())

;;; arithmetic operators

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *op-precedence-hash* (make-hash-table :test #'equal))

  ;;; generate the operator precedences from *OP-PRECEDENCES*
  (let ((precedence 1))
    (dolist (ops '((aref)
                   (slot-value)
                   (! not ~)
                   (* / %)
                   (+ -)
                   (<< >>)
                   (>>>)
                   (< > <= >=)
                   (in if)
                   (eql == != =)
                   (=== !==)
                   (&)
                   (^)
                   (\|)
                   (\&\& and)
                   (\|\| or)
                   (setf *= /= %= += -= <<= >>= >>>= \&= ^= \|=)
                   (comma)))
      (dolist (op ops)
        (let ((op-name (symbol-name op)))
          (setf (gethash op-name *op-precedence-hash*) precedence)))
      (incf precedence)))

  (defun op-precedence (op)
    (gethash (if (symbolp op)
                 (symbol-name op)
                 op)
             *op-precedence-hash*)))

(defun js-convert-op-name (op)
  (case op
    (and '\&\&)
    (or '\|\|)
    (not '!)
    (eql '\=\=)
    (=   '\=\=)
    (t op)))

(defjsclass op-form (expression)
  ((operator :initarg :operator :accessor operator)
   (args :initarg :args :accessor op-args)))

(defun op-form-p (form)
  (and (listp form)
       (not (js-compiler-macro-form-p form))
       (not (null (op-precedence (first form))))))

(defun klammer (string-list)
  (prepend-to-first string-list "(")
  (append-to-last string-list ")")
  string-list)

(defmethod expression-precedence ((expression expression))
  0)

(defmethod expression-precedence ((form op-form))
  (op-precedence (operator form)))

(defmethod js-to-strings ((form op-form) start-pos)
  (let* ((precedence (expression-precedence form))
	 (value-string-lists
	  (mapcar #'(lambda (x)
		      (let ((string-list (js-to-strings x (+ start-pos 2))))
			(if (>= (expression-precedence x) precedence)
			    (klammer string-list)
			    string-list)))
		  (op-args form)))
	 (max-length (- 80 start-pos 2))
	 (op-string (format nil "~A " (operator form))))
    (dwim-join value-string-lists max-length :join-before op-string)    
    ))

(defjsmacro 1- (form)
  `(- ,form 1))

(defjsmacro 1+ (form)
  `(+ ,form 1))

(defjsclass one-op (expression)
  ((pre-p :initarg :pre-p
	  :initform nil
	  :accessor one-op-pre-p)
   (op :initarg :op
       :accessor one-op)))

(defmethod js-to-strings ((one-op one-op) start-pos)
  (let* ((value (value one-op))
	 (value-strings (js-to-strings value start-pos)))
    (when (typep value 'op-form)
      (setf value-strings (klammer value-strings)))
    (if (one-op-pre-p one-op)
      (prepend-to-first value-strings
			(one-op one-op))
      (append-to-last value-strings
		      (one-op one-op)))))

(define-js-compiler-macro ++ (x)
  (make-instance 'one-op :pre-p nil :op "++"
		 :value (js-compile-to-expression x)))

(define-js-compiler-macro -- (x)
  (make-instance 'one-op :pre-p nil :op "--"
		 :value (js-compile-to-expression x)))

(define-js-compiler-macro incf (x &optional (delta 1))
  (if (eql delta 1)
      (make-instance 'one-op :pre-p t :op "++"
                     :value (js-compile-to-expression x))
      (make-instance 'op-form
                     :operator '+=
                     :args (mapcar #'js-compile-to-expression
                                   (list x delta )))))

(define-js-compiler-macro decf (x &optional (delta 1))
  (if (eql delta 1)
      (make-instance 'one-op :pre-p t :op "--"
                     :value (js-compile-to-expression x))
      (make-instance 'op-form
                     :operator '-=
                     :args (mapcar #'js-compile-to-expression
                                   (list x delta )))))

(define-js-compiler-macro - (first &rest rest)
  (if (null rest)
      (make-instance 'one-op
                     :pre-p t
                     :op "-"
                     :value (js-compile-to-expression first))
      (make-instance 'op-form
                     :operator '-
                     :args (mapcar #'js-compile-to-expression
                                   (cons first rest)))))

(define-js-compiler-macro not (x)
  (let ((value (js-compile-to-expression x)))
    (if (and (typep value 'op-form)
	     (= (length (op-args value)) 2))
	(let ((new-op (case (operator value)
			(== '!=)
			(< '>=)
			(> '<=)
			(<= '>)
			(>= '<)
			(!= '==)
			(=== '!==)
			(!== '===)
			(t nil))))
	  (if new-op
	      (make-instance 'op-form :operator new-op
			     :args (op-args value))
	      (make-instance 'one-op :pre-p t :op "!"
			    :value value)))
	(make-instance 'one-op :pre-p t :op "!"
		       :value value))))

(define-js-compiler-macro ~ (x)
  (let ((expr (js-compile-to-expression x)))
    (make-instance 'one-op :pre-p t :op "~" :value expr)))

;;; function calls

(defjsclass function-call (expression)
  ((function :initarg :function :accessor f-function)
   (args :initarg :args :accessor f-args)))

(defun funcall-form-p (form)
  (and (listp form)
       (not (op-form-p form))
       (not (js-compiler-macro-form-p form))))

(defmethod js-to-strings ((form function-call) start-pos)
  (let* ((value-string-lists
	  (mapcar #'(lambda (x) (js-to-strings x (+ start-pos 2)))
		  (f-args form)))
	 (max-length (- 80 start-pos 2))
	 (args (dwim-join value-string-lists max-length
			  :start "(" :end ")" :join-after ",")))
    (etypecase (f-function form)
      (js-lambda
       (dwim-join (list (append (dwim-join (list (js-to-strings (f-function form) (+ start-pos 2)))
                                           max-length
                                           :start "(" :end ")" :separator "")
                                args))
                  max-length
                  :separator ""))
      ((or js-variable js-aref js-slot-value)
       (dwim-join (list (js-to-strings (f-function form) (+ start-pos 2))
                        args)
                  max-length
                  :separator ""))
      (function-call
       ;; TODO it adds superfluous newlines after each ()
       ;; and it's nearly the same as the js-lambda case above
       (dwim-join (list (append (dwim-join (list (js-to-strings (f-function form) (+ start-pos 2)))
                                           max-length :separator "")
                                args))
                  max-length :separator "")))))

(defjsclass method-call (expression)
  ((method :initarg :method :accessor m-method)
   (object :initarg :object :accessor m-object)
   (args :initarg :args :accessor m-args)))

(defmethod js-to-strings ((form method-call) start-pos)
  (let ((object (js-to-strings (m-object form) (+ start-pos 2))))
    ;; TODO: this may not be the best way to add ()'s around lambdas
    ;; probably there is or should be a more general solution working
    ;; in other situations involving lambda's
    (when (member (m-object form) (list 'js-lambda 'number-literal 'js-object 'op-form) :test #'typep)  
      (push "(" object)
      (nconc object (list ")")))
    (let* ((fname (dwim-join (list object
                                   (list (symbol-to-js (m-method form))))
                             (- 80 start-pos 2)
                             :end "("
                             :separator ""))
           (butlast (butlast fname))
           (last (car (last fname)))
           (method-and-args (dwim-join (mapcar #'(lambda (x) (js-to-strings x (+ start-pos 2)))
                                               (m-args form))
                                       (- 80 start-pos 2)
                                       :start last
                                       :end ")"
                                       :join-after ","))
           (ensure-no-newline-before-dot (concatenate 'string
                                                      (car (last butlast))
                                                      (first method-and-args))))
      (nconc (butlast butlast)
             (list ensure-no-newline-before-dot)
             (rest method-and-args)))))

(defun method-call-p (form)
  (and (funcall-form-p form)
       (symbolp (first form))
       (eql (char (symbol-name (first form)) 0) #\.)))

;;; body forms

(defjsclass js-body (expression)
  ((stmts :initarg :stmts :accessor b-stmts)
   (indent :initarg :indent :initform "" :accessor b-indent)))

(define-js-compiler-macro progn (&rest body)
  (make-instance 'js-body
		 :stmts (mapcar #'js-compile-to-statement body)))

(defmethod initialize-instance :after ((body js-body) &rest initargs)
  (declare (ignore initargs))
  (let* ((stmts (b-stmts body))
	 (last (last stmts))
	 (last-stmt (car last)))
    (when (typep last-stmt 'js-body)
      (setf (b-stmts body)
	    (nconc (butlast stmts)
		   (b-stmts last-stmt))))))


(defmethod js-to-statement-strings ((body js-body) start-pos)
  (dwim-join (mapcar #'(lambda (x) (js-to-statement-strings x (+ start-pos 2)))
		     (b-stmts body))
	     (- 80 start-pos 2)
	     :join-after ";"
	     :append-to-last #'special-append-to-last
	     :start (b-indent body) :collect nil
	     :end ";"))

(defmethod js-to-strings ((body js-body) start-pos)
  (dwim-join (mapcar #'(lambda (x) (js-to-strings x (+ start-pos 2)))
		     (b-stmts body))
	     (- 80 start-pos 2)
	     :append-to-last #'special-append-to-last
	     :join-after ","
	     :start (b-indent body)))

(defjsclass js-sub-body (js-body)
  (stmts indent))

(defmethod js-to-statement-strings ((body js-sub-body) start-pos)
  (declare (ignore start-pos))
  (nconc (list "{") (call-next-method) (list "}")))

(defmethod expression-precedence ((body js-body))
  (if (= (length (b-stmts body)) 1)
      (expression-precedence (first (b-stmts body)))
      (op-precedence 'comma)))

;;; function definition

(defjsclass js-lambda (expression)
  ((args :initarg :args :accessor lambda-args)
   (body :initarg :body :accessor lambda-body)))

(define-js-compiler-macro lambda (args &rest body)
  (make-instance 'js-lambda
                 :args (mapcar #'js-compile-to-symbol args)
                 :body (make-instance 'js-body
                                      :indent "  "
                                      :stmts (mapcar #'js-compile-to-statement body))))

(defmethod js-to-strings ((lambda js-lambda) start-pos)
  (let ((fun-header (dwim-join (mapcar #'(lambda (x)
                                           (list (symbol-to-js x)))
				       (lambda-args lambda))
			       (- 80 start-pos 2)
			       :start (function-start-string lambda)
			       :end ") {" :join-after ","))
	(fun-body (js-to-statement-strings (lambda-body lambda) (+ start-pos 2))))
    (nconc fun-header fun-body (list "}"))))

(defmethod function-start-string ((lambda js-lambda))
  "function (")

(defmethod js-to-statement-strings ((lambda js-lambda) start-pos)
  (js-to-strings lambda start-pos))

(defjsclass js-defun (js-lambda)
  ((name :initarg :name :accessor defun-name)))

(define-js-compiler-macro defun (name args &rest body)
  (make-instance 'js-defun
		 :name (js-compile-to-symbol name)
		 :args (mapcar #'js-compile-to-symbol args)
		 :body (make-instance 'js-body
				      :indent "  "
				      :stmts (mapcar #'js-compile-to-statement body))))

(defmethod function-start-string ((defun js-defun))
  (format nil "function ~A(" (symbol-to-js (defun-name defun))))

;;; object creation

(defjsclass js-object (expression)
  ((slots :initarg :slots
	  :accessor o-slots)))

(define-js-compiler-macro create (&rest args)
  (make-instance 'js-object
		 :slots (loop for (name val) on args by #'cddr
			      collect (let ((name-expr (js-compile-to-expression name)))
					(assert (or (typep name-expr 'js-variable)
						    (typep name-expr 'string-literal)
						    (typep name-expr 'number-literal)))
					(list name-expr (js-compile-to-expression val))))))

(defmethod js-to-strings ((object js-object) start-pos)
  (let ((value-string-lists
	 (mapcar #'(lambda (slot)
		     (dwim-join (list (js-to-strings (second slot) (+ start-pos 4)))
				(- 80 start-pos 2)
				:start (concatenate 'string (car (js-to-strings (first slot) 0)) " : ")
				:white-space "    ")) (o-slots object)))
	(max-length (- 80 start-pos 2)))
    (dwim-join value-string-lists max-length
	       :start "{ "
	       :end " }"
	       :join-after ", "
	       :white-space "  "
	       :collect nil)))

(defjsclass js-slot-value (expression)
  ((object :initarg :object
	   :accessor sv-object)
   (slot :initarg :slot
	 :accessor sv-slot)))

(define-js-compiler-macro slot-value (obj slot)
  (make-instance 'js-slot-value :object (js-compile-to-expression obj)
   		   :slot (js-compile slot)))

(defmethod js-to-strings ((sv js-slot-value) start-pos)
  (append-to-last (js-to-strings (sv-object sv) start-pos)
                  (if (typep (sv-slot sv) 'js-quote)
                      (if (symbolp (value (sv-slot sv)))
                          (format nil ".~A" (symbol-to-js (value (sv-slot sv))))
                          (format nil ".~A" (first (js-to-strings (sv-slot sv) 0))))
                      (format nil "[~A]" (first (js-to-strings (sv-slot sv) 0))))))

(defjsmacro with-slots (slots object &rest body)
  `(symbol-macrolet ,(mapcar #'(lambda (slot)
				 `(,slot '(slot-value ,object ',slot)))
			     slots)
    ,@body))

;;; macros

(define-js-compiler-macro macrolet (macros &rest body)
  (let* ((macro-env (make-hash-table :test 'equal))
	 (*js-macro-env* (cons macro-env *js-macro-env*)))
    (dolist (macro macros)
      (destructuring-bind (name arglist &rest body) macro
	(setf (gethash (symbol-name name) macro-env)
	      (compile nil `(lambda ,arglist ,@body)))))
    (js-compile `(progn ,@body))))

(defjsmacro symbol-macrolet (macros &rest body)
  `(macrolet ,(mapcar #'(lambda (macro)
			  `(,(first macro) () ,@(rest macro))) macros)
    ,@body))

;;; lisp eval

(defjsmacro lisp (&rest forms)
  (eval (cons 'progn forms)))

;;; cond

(defjsclass js-cond (expression)
  ((tests :initarg :tests
	  :accessor cond-tests)
   (bodies :initarg :bodies
	   :accessor cond-bodies)))

(define-js-compiler-macro cond (&rest clauses)
  (make-instance 'js-cond
		 :tests (mapcar (lambda (clause) (js-compile-to-expression (car clause)))
				clauses)
		 :bodies (mapcar (lambda (clause) (js-compile-to-body (cons 'progn (cdr clause)) :indent "  "))
				 clauses)))

(defmethod js-to-statement-strings ((cond js-cond) start-pos)
  (loop :for body :on (cond-bodies cond)
	:for first = (eq body (cond-bodies cond))
	:for last = (not (cdr body))
	:for test :in (cond-tests cond)
	:append (if (and last (not first) (string= (value test) "true"))
		    '("else {")
		    (dwim-join (list (js-to-strings test 0)) (- 80 start-pos 2)
			       :start (if first "if (" "else if (") :end ") {"))
	:append (js-to-statement-strings (car body) (+ start-pos 2))
	:collect "}"))

;;; if

(defjsclass js-if (expression)
  ((test :initarg :test
	 :accessor if-test)
   (then :initarg :then
	 :accessor if-then)
   (else :initarg :else
	 :accessor if-else)))

(define-js-compiler-macro if (test then &optional else)
  (make-instance 'js-if :test (js-compile-to-expression test)
		 :then (js-compile-to-body then :indent "  ")
		 :else (when else
			 (js-compile-to-body else :indent "  "))))

(defmethod initialize-instance :after ((if js-if) &rest initargs)
  (declare (ignore initargs))
  (when (and (if-then if)
	     (typep (if-then if) 'js-sub-body))
    (change-class (if-then if) 'js-body))
  (when (and (if-else if)
	     (typep (if-else if) 'js-sub-body))
    (change-class (if-else if) 'js-body)))

(defmethod js-to-statement-strings ((if js-if) start-pos)
  (let ((if-strings (dwim-join (list (js-to-strings (if-test if) 0))
			       (- 80 start-pos 2)
			       :start "if ("
			       :end ") {"))
	(then-strings (js-to-statement-strings (if-then if) (+ start-pos 2)))
	(else-strings (when (if-else if)
			(js-to-statement-strings (if-else if)
						 (+ start-pos 2)))))
    (nconc if-strings then-strings (if else-strings
				       (nconc (list "} else {") else-strings (list "}"))
				       (list "}")))))

(defmethod expression-precedence ((if js-if))
  (op-precedence 'if))

(defmethod js-to-strings ((if js-if) start-pos)
  (assert (typep (if-then if) 'expression))
  (when (if-else if)
    (assert (typep (if-else if) 'expression)))
  (dwim-join (list (append-to-last (js-to-strings (if-test if) start-pos) " ?")
		   (let* ((new-then (make-instance 'js-body
						   :stmts (b-stmts (if-then if))
						   :indent ""))
			  (res (js-to-strings new-then start-pos)))
		     (if (>= (expression-precedence (if-then if))
			     (expression-precedence if))
			     (klammer res)
			     res))
		   (list ":")
		   (if (if-else if)
		       (let* ((new-else (make-instance 'js-body
						       :stmts (b-stmts (if-else if))
						       :indent ""))
			      (res (js-to-strings new-else start-pos)))
			 (if (>= (expression-precedence (if-else if))
				 (expression-precedence if))
			     (klammer res)
			     res))
		       (list "undefined")))
	     (- 80 start-pos 2)
	     :white-space "  "))

(defjsmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defjsmacro unless (test &rest body)
  `(if (not ,test) (progn ,@body)))

;;; single keyword expressions and statements

(defmacro define-js-single-op (name &optional (superclass 'expression))
  (let ((js-name (intern (concatenate 'string "JS-" (symbol-name name)) #.*package*)))
  `(progn
    (defjsclass ,js-name (,superclass)
      (value))
    (define-js-compiler-macro ,name (value)
      (make-instance ',js-name :value (js-compile-to-expression value)))
    (defmethod ,(if (eql superclass 'expression)
		    'js-to-strings
		    'js-to-statement-strings) ((,name ,js-name) start-pos)
      (dwim-join (list (js-to-strings (value ,name) (+ start-pos 2)))
		 (- 80 start-pos 2)
		 :start ,(concatenate 'string (string-downcase (symbol-name name)) " ")
		 :white-space "  ")))))


(define-js-single-op return statement)
(define-js-single-op throw statement)
(define-js-single-op delete)
(define-js-single-op void)
(define-js-single-op typeof)
(define-js-single-op new)

;; TODO this may not be the best integrated implementation of
;; instanceof into the rest of the code
(defjsclass js-instanceof (expression)
  ((value)
   (type :initarg :type)))

(define-js-compiler-macro instanceof (value type)
  (make-instance 'js-instanceof
                 :value (js-compile-to-expression value)
                 :type (js-compile-to-expression type)))

(defmethod js-to-strings ((instanceof js-instanceof) start-pos)
  (dwim-join
   (list (js-to-strings (value instanceof) (+ start-pos 2))
         (list "instanceof")
         (js-to-strings (slot-value instanceof 'type) (+ start-pos 2)))
   (- 80 start-pos 2)
   :start "("
   :end ")"
   :white-space
   "  "))

;;; assignment

(defjsclass js-setf (expression)
  ((lhs :initarg :lhs :accessor setf-lhs)
   (rhsides :initarg :rhsides :accessor setf-rhsides)))

(defun assignment-op (op)
  (case op
    (+ '+=)
    (~ '~=)
    (\& '\&=)
    (\| '\|=)
    (- '-=)
    (* '*=)
    (% '%=)
    (>> '>>=)
    (^  '^=)
    (<< '<<=)
    (>>> '>>>=)
    (/   '/=)
    (t   nil)))

(defun make-js-test (lhs rhs)
  (if (and (typep rhs 'op-form)
	   (member lhs (op-args rhs) :test #'js-equal))
      (let ((args-without (remove lhs (op-args rhs)
				  :count 1 :test #'js-equal))
	    (args-without-first (remove lhs (op-args rhs)
					:count 1 :end 1
					:test #'js-equal))
	    (one (list (make-instance 'number-literal :value 1))))
	#+nil
	(format t "OPERATOR: ~S, ARGS-WITHOUT: ~S, ARGS-WITHOUT-FIRST ~S~%"
		(operator rhs)
		args-without
		args-without-first)
	(cond ((and (js-equal args-without one)
		    (eql (operator rhs) '+))
	       (make-instance 'one-op :pre-p nil :op "++"
			      :value lhs))
	      ((and (js-equal args-without-first one)
		    (eql (operator rhs) '-))
	       (make-instance 'one-op :pre-p nil :op "--"
			      :value lhs))
	      ((and (assignment-op (operator rhs))
		    (member (operator rhs)
			    '(+ *))
                    (js-equal lhs (first (op-args rhs))))
	       (make-instance 'op-form
			      :operator (assignment-op (operator rhs))
			      :args (list lhs (make-instance 'op-form
							     :operator (operator rhs)
							     :args args-without-first))))
	      ((and (assignment-op (operator rhs))
		    (js-equal (first (op-args rhs)) lhs))
	       (make-instance 'op-form
			      :operator (assignment-op (operator rhs))
			      :args (list lhs (make-instance 'op-form
							     :operator (operator rhs)
							     :args (cdr (op-args rhs))))))
	      (t (make-instance 'js-setf :lhs lhs :rhsides (list rhs)))))
      (make-instance 'js-setf :lhs lhs :rhsides (list rhs))))

(define-js-compiler-macro setf (&rest args)
  (let ((assignments (loop for (lhs rhs) on args by #'cddr
			   for rexpr = (js-compile-to-expression rhs)
			   for lexpr = (js-compile-to-expression lhs)
			   collect (make-js-test lexpr rexpr))))
    (if (= (length assignments) 1)
	(first assignments)
	(make-instance 'js-body :indent "" :stmts assignments))))

(defmethod js-to-strings ((setf js-setf) start-pos)
  (dwim-join (cons (js-to-strings (setf-lhs setf) start-pos)
		   (mapcar #'(lambda (x) (js-to-strings x start-pos)) (setf-rhsides setf)))
	     (- 80 start-pos 2)
	     :join-after " ="))

(defmethod expression-precedence ((setf js-setf))
  (op-precedence '=))

;;; defvar

(defjsclass js-defvar (statement)
  ((names :initarg :names :accessor var-names)
   (value :initarg :value :accessor var-value)))

(define-js-compiler-macro defvar (name &optional value)
  (make-instance 'js-defvar :names (list (js-compile-to-symbol name))
		 :value (when value (js-compile-to-expression value))))

(defmethod js-to-statement-strings ((defvar js-defvar) start-pos)
  (dwim-join (nconc (mapcar #'(lambda (x) (list (symbol-to-js x))) (var-names defvar))
		    (when (var-value defvar)
		      (list (js-to-strings (var-value defvar) start-pos))))
	     (- 80 start-pos 2)
	     :join-after " ="
	     :start "var " :end ";"))

;;; let

(define-js-compiler-macro let (decls &rest body)
  (let ((defvars (mapcar #'(lambda (decl)
			     (if (atom decl)
                                 (make-instance 'js-defvar
                                       :names (list (js-compile-to-symbol decl))
                                       :value nil)
                                 (let ((name (first decl))
                                       (value (second decl)))
                                   (make-instance 'js-defvar
                                                  :names (list (js-compile-to-symbol name))
                                                  :value (js-compile-to-expression value)))))
			 decls)))
    (make-instance 'js-sub-body
		   :indent "  "
		   :stmts (nconc defvars
				 (mapcar #'js-compile-to-statement body)))))

;;; iteration

(defjsclass js-for (statement)
  ((vars :initarg :vars :accessor for-vars)
   (steps :initarg :steps :accessor for-steps)
   (check :initarg :check :accessor for-check)
   (body :initarg :body :accessor for-body)))

(defun make-for-vars (decls)
  (loop for decl in decls
	for var = (if (atom decl) decl (first decl))
	for init = (if (atom decl) nil (second decl))
	collect (make-instance 'js-defvar :names (list (js-compile-to-symbol var))
			       :value (js-compile-to-expression init))))

(defun make-for-steps (decls)
  (loop for decl in decls
	when (= (length decl) 3)
	collect (js-compile-to-expression (third decl))))

(define-js-compiler-macro do (decls termination &rest body)
  (let ((vars (make-for-vars decls))
	(steps (make-for-steps decls))
	(check (js-compile-to-expression (list 'not (first termination))))
	(body (js-compile-to-body (cons 'progn body) :indent "  ")))
    (make-instance 'js-for
		   :vars vars
		   :steps steps
		   :check check
		   :body body)))

(defjsmacro dotimes (iter &rest body)
  (let ((var (first iter))
        (times (second iter)))
  `(do ((,var 0 (1+ ,var)))
       ((>= ,var ,times))
     ,@body)))

(defjsmacro dolist (i-array &rest body)
  (let ((var (first i-array))
	(array (second i-array))
	(arrvar (js-gensym "arr"))
	(idx (js-gensym "i")))
    `(let ((,arrvar ,array))
      (do ((,idx 0 (1+ ,idx)))
	  ((>= ,idx (slot-value ,arrvar 'length)))
	(let ((,var (aref ,arrvar ,idx)))
	  ,@body)))))

(defmethod js-to-statement-strings ((for js-for) start-pos)
  (let* ((init (dwim-join (mapcar #'(lambda (x)
				      (dwim-join (list (list (symbol-to-js (first (var-names x))))
						       (js-to-strings (var-value x)
								      (+ start-pos 2)))
						 (- 80 start-pos 2)
						 :join-after " ="))
				  (for-vars for))
			  (- 80 start-pos 2)
			  :start "var " :join-after ","))
	 (check (js-to-strings (for-check for) (+ start-pos 2)))
	 (steps (dwim-join (mapcar #'(lambda (x var)
				       (dwim-join
					(list (list (symbol-to-js (first (var-names var))))
					      (js-to-strings x (- start-pos 2)))
					(- 80 start-pos 2)
					:join-after " ="))
				   (for-steps for)
				   (for-vars for))
			   (- 80 start-pos 2)
			   :join-after ","))
	 (header (dwim-join (list init check steps)
			    (- 80 start-pos 2)
			    :start "for (" :end ") {"
			    :join-after ";"))
	 (body (js-to-statement-strings (for-body for) (+ start-pos 2))))
    (nconc header body (list "}"))))

(defjsclass for-each (statement)
  ((name :initarg :name :accessor fe-name)
   (value :initarg :value :accessor fe-value)
   (body :initarg :body :accessor fe-body)))

(define-js-compiler-macro doeach (decl &rest body)
  (make-instance 'for-each :name (js-compile-to-symbol (first decl))
		 :value (js-compile-to-expression (second decl))
		 :body (js-compile-to-body (cons 'progn body) :indent "  ")))

(defmethod js-to-statement-strings ((fe for-each) start-pos)
  (let ((header (dwim-join (list (list (symbol-to-js (fe-name fe)))
				 (list "in")
				 (js-to-strings (fe-value fe) (+ start-pos 2)))
			   (- 80 start-pos 2)
			   :start "for (var "
			   :end ") {"))
	(body (js-to-statement-strings (fe-body fe) (+ start-pos 2))))
    (nconc header body (list "}"))))

(defjsclass js-while (statement)
  ((check :initarg :check :accessor while-check)
   (body :initarg :body :accessor while-body)))

(define-js-compiler-macro while (check &rest body)
  (make-instance 'js-while
		 :check (js-compile-to-expression check)
		 :body (js-compile-to-body (cons 'progn body) :indent "  ")))

(defmethod js-to-statement-strings ((while js-while) start-pos)
  (let ((header (dwim-join (list (js-to-strings (while-check while) (+ start-pos 2)))
			   (- 80 start-pos 2)
			   :start "while ("
			   :end ") {"))
	(body (js-to-statement-strings (while-body while) (+ start-pos 2))))
    (nconc header body (list "}"))))

;;; with

(defjsclass js-with (statement)
  ((obj :initarg :obj :accessor with-obj)
   (body :initarg :body :accessor with-body)))

(define-js-compiler-macro with (statement &rest body)
  (make-instance 'js-with
                 :obj (js-compile-to-expression statement)
                 :body (js-compile-to-body (cons 'progn body) :indent "  ")))

(defmethod js-to-statement-strings ((with js-with) start-pos)
  (nconc (dwim-join (list (js-to-strings (with-obj with) (+ start-pos 2)))
		    (- 80 start-pos 2)
		    :start "with (" :end ") {")
	 (js-to-statement-strings (with-body with) (+ start-pos 2))
	 (list "}")))

;;; case

(defjsclass js-switch (statement)
  ((value :initarg :value :accessor case-value)
   (clauses :initarg :clauses :accessor case-clauses)))

(define-js-compiler-macro switch (value &rest clauses)
  (let ((clauses (mapcar #'(lambda (clause)
			     (let ((val (first clause))
				   (body (cdr clause)))
			       (list (if (eql val 'default)
					 'default
					 (js-compile-to-expression val))
				     (js-compile-to-body (cons 'progn body) :indent "  "))))
			 clauses))
	(check (js-compile-to-expression value)))
    (make-instance 'js-switch :value check
		   :clauses clauses)))

(defmethod js-to-statement-strings ((case js-switch) start-pos)
  (let ((body 	 (mapcan #'(lambda (clause)
		     (let ((val (car clause))
			   (body (second clause)))
		       (dwim-join (list (if (eql val 'default)
					    (list "")
					    (js-to-strings val (+ start-pos 2)))
					(js-to-statement-strings body (+ start-pos 2)))
				  (- 80 start-pos 2)
				  :start (if (eql val 'default) "  default" "  case ")
				  :white-space "   "
				  :join-after ":"))) (case-clauses case))))

    #+nil
    (format t "body: ~S~%" body)
    (nconc (dwim-join (list (js-to-strings (case-value case) (+ start-pos 2)))
		    (- 80 start-pos 2)
		    :start "switch (" :end ") {")
	   body
	   (list "}"))))

(defjsmacro case (value &rest clauses)
  (labels ((make-clause (val body more)
             (cond ((listp val)
                    (append (mapcar #'list (butlast val))
                            (make-clause (first (last val)) body more)))
                   ((member val '(t otherwise))
                    (make-clause 'default body more))
                   (more `((,val ,@body break)))
                   (t `((,val ,@body))))))
    `(switch ,value ,@(mapcon #'(lambda (x)
                                  (make-clause (car (first x))
                                               (cdr (first x))
                                               (rest x)))
                              clauses))))

;;; throw catch

(defjsclass js-try (statement)
  ((body :initarg :body :accessor try-body)
   (catch :initarg :catch :accessor try-catch)
   (finally :initarg :finally :accessor try-finally)))

(define-js-compiler-macro try (body &rest clauses)
  (let ((body (js-compile-to-body body :indent "  "))
	(catch (cdr (assoc :catch clauses)))
	(finally (cdr (assoc :finally clauses))))
    (assert (not (cdar catch)) nil "Sorry, currently only simple catch forms are supported.")
    (make-instance 'js-try
		   :body body
		   :catch (when catch (list (js-compile-to-symbol (caar catch))
					    (js-compile-to-body (cons 'progn (cdr catch))
								:indent "  ")))
		   :finally (when finally (js-compile-to-body (cons 'progn finally)
							      :indent "  ")))))

(defmethod js-to-statement-strings ((try js-try) start-pos)
  (let* ((catch (try-catch try))
	 (finally (try-finally try))
	 (catch-list (when catch
		       (nconc
			(dwim-join (list (list (symbol-to-js (first catch))))
				   (- 80 start-pos 2)
				   :start "} catch ("
				   :end ") {")
			(js-to-statement-strings (second catch) (+ start-pos 2)))))
	 (finally-list (when finally
			 (nconc (list "} finally {")
				(js-to-statement-strings finally (+ start-pos 2))))))
    (nconc (list "try {")
	   (js-to-statement-strings (try-body try) (+ start-pos 2))
	   catch-list
	   finally-list
	   (list "}"))))

;;; regex

(defjsclass regex (expression)
  (value))

(define-js-compiler-macro regex (regex)
  (make-instance 'regex :value (string regex)))

(defun first-slash-p (string)
  (and (> (length string) 0)
       (eq (char string 0) '#\/)))

(defmethod js-to-strings ((regex regex) start-pos)
   (declare (ignore start-pos))
   (let ((slash (if (first-slash-p (value regex)) nil "/")))
     (list (format nil (concatenate 'string slash "~A" slash) (value regex)))))

;;; conditional compilation

(defjsclass cc-if ()
  ((test :initarg :test :accessor cc-if-test)
   (body :initarg :body :accessor cc-if-body)))

(defmethod js-to-statement-strings ((cc cc-if) start-pos)
  (nconc (list (format nil "/*@if ~A" (cc-if-test cc)))
	 (mapcan #'(lambda (x) (js-to-strings x start-pos)) (cc-if-body cc))
	 (list "@end @*/")))

(define-js-compiler-macro cc-if (test &rest body)
  (make-instance 'cc-if :test test
		 :body (mapcar #'js-compile body)))

;;; compiler

(defun js-compile (form)
  (setf form (js-expand-form form))
  (cond ((stringp form)
	 (make-instance 'string-literal :value form))
        ((characterp form)
	 (make-instance 'string-literal :value (string form)))
	((numberp form)
	 (make-instance 'number-literal :value form))
	((symbolp form)
	 (let ((c-macro (js-get-compiler-macro form)))
	   (if c-macro
	       (funcall c-macro)
	       (make-instance 'js-variable :value form))))
	((and (consp form)
	      (eql (first form) 'quote))
	 (make-instance 'js-quote :value (second form)))
	((consp form)
	 (js-compile-list form))
	(t (error "Unknown atomar expression ~S" form))))

(defun js-compile-list (form)
  (let* ((name (car form))
	 (args (cdr form))
	 (js-form (js-get-compiler-macro name)))
    (cond (js-form
	   (apply js-form args))

	  ((op-form-p form)
	   (make-instance 'op-form
			  :operator (js-convert-op-name (js-compile-to-symbol (first form)))
			  :args (mapcar #'js-compile-to-expression (rest form))))

	  ((method-call-p form)
	   (make-instance 'method-call
			  :method (js-compile-to-symbol (first form))
			  :object (js-compile-to-expression (second form))
			  :args (mapcar #'js-compile-to-expression (cddr form))))

	  ((funcall-form-p form)
	   (make-instance 'function-call
			  :function (js-compile-to-expression (first form))
			  :args (mapcar #'js-compile-to-expression (rest form))))

	  (t (error "Unknown form ~S" form)))))

(defun js-compile-to-expression (form)
  (let ((res (js-compile form)))
    (assert (typep res 'expression))
    res))

(defun js-compile-to-symbol (form)
  (let ((res (js-compile form)))
    (when (typep res 'js-variable )
      (setf res (value res)))
    (assert (symbolp res))
    res))

(defun js-compile-to-statement (form)
  (let ((res (js-compile form)))
    (assert (typep res 'statement))
    res))

(defun js-compile-to-body (form &key (indent ""))
  (let ((res (js-compile-to-statement form)))
    (if (typep res 'js-body)
	(progn (setf (b-indent res) indent)
	       res)
	(make-instance 'js-body
		       :indent indent
		       :stmts (list res)))))

;;; Math library

(defjsmacro floor (expr)
  `(*Math.floor ,expr))

(defjsmacro random ()
  `(*Math.random))

(defjsmacro evenp (num)
  `(= (% ,num 2) 0))

(defjsmacro oddp (num)
  `(= (% ,num 2) 1))

;;; helper macros

(define-js-compiler-macro js (&rest body)
  (make-instance 'string-literal
		 :value (string-join (js-to-statement-strings
				      (js-compile (cons 'progn body)) 0) " ")))

(define-js-compiler-macro js-inline (&rest body)
  (make-instance 'string-literal
		 :value (concatenate
			 'string
			 "javascript:"
			 (string-join (js-to-statement-strings
				       (js-compile (cons 'progn body)) 0) " "))))


(defmacro js (&rest body)
  `(js* '(progn ,@body)))

(defmacro js* (&rest body)
  "Return the javascript string representing BODY.

Body is evaluated."
  `(string-join
    (js-to-statement-strings (js-compile (list 'progn ,@body)) 0)
    (string #\Newline)))

(defun js-to-string (expr)
  (string-join
   (js-to-statement-strings (js-compile expr) 0)
   (string #\Newline)))

(defun js-to-line (expr)
  (string-join
   (js-to-statement-strings (js-compile expr) 0) " "))

(defmacro js-file (&rest body)
  `(html
    (:princ
     (js ,@body))))

(defmacro js-script (&rest body)
  `((:script :type "text/javascript")
    (:princ (format nil "~%// <![CDATA[~%"))
    (:princ (js ,@body))
    (:princ (format nil "~%// ]]>~%"))))

(defmacro js-inline (&rest body)
  `(js-inline* '(progn ,@body)))

(defmacro js-inline* (&rest body)
  "Just like JS-INLINE except that BODY is evaluated before being
converted to javascript."
  `(concatenate 'string "javascript:"
    (string-join (js-to-statement-strings (js-compile (list 'progn ,@body)) 0) " ")))

