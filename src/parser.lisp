(in-package :parenscript)

;;;; The mechanisms for defining macros & parsing Parenscript.

(defclass identifier ()
  ((symbol :accessor id-symbol :initform nil :type symbol))
  (:documentation ""))

(defclass script-package ()
  ;; configuration slots
  ((name          :accessor script-package-name          :initform nil :initarg :name :type string
		  :documentation "Canonical name of the package (a String).")
   (nicknames     :accessor script-package-nicknames     :initform nil :initarg :nicknames
		  :documentation "List of nicknames for the package (as strings).")
   (lisp-package  :accessor script-package-lisp-package  :initform nil :initarg :lisp-package)
   (secondary-lisp-packages :accessor script-package-secondary-lisp-packages :initform nil
			    :initarg :secondary-lisp-packages)
   (exports       :accessor script-package-exports       :initform nil :initarg :exports
		  :documentation "List of exported identifiers.")
   (used-packages :accessor script-package-used-packages :initform nil :initarg :used-packages
		  :documentation "")
   (documentation :accessor script-package-documentation :initform nil :initarg :documentation)
   (compilation-env :accessor script-package-comp-env    :initform nil :initarg :comp-env)
   (locked?       :accessor script-package-locked? :initform nil :initarg :locked?
		  :documentation "t if redefinition of top-level symbols is disallowed.")
   ;; internal use slots
   (exclusive-lisp-package-p
    :initform nil :initarg :exclusive-lisp-package?
    :accessor script-package-exclusive-lisp-package-p
    :documentation "t if the lisp package is an anonymous package created exclusively for
                    the script package.")
   (toplevel-identifiers :accessor script-package-toplevel-ids :initarg :toplevel-ids
                         :initform nil)
   (macro-table   :accessor script-package-macro-table
                  :initform (make-hash-table :test #'eql)
                  :documentation "This package's macro environment, set up as a hash table
                                  from symbols to macro functions")
   (special-form-table :accessor script-package-special-form-table
		       :initform (make-hash-table :test #'equal)
		       :documentation "Holds special form macros for the package.
                                       Probably not used except for built-in packages."))
  (:documentation "A Parenscript package is a lisp object that holds information
about a set of Suavescript code."))

(defclass compilation-environment ()
  ((script-packages :accessor comp-env-script-packages :initform nil :initarg :packages
		    :documentation "List of packages defined in this environment.")
   (current-package :accessor comp-env-current-package :initform nil :initarg :current-package
		    :documentation "Current in-package.")
   (lisp-to-script-package-table
    :accessor comp-env-lisp-to-script-package-table :initform (make-hash-table)
    :documentation   "Maps a lisp package to a script package."))
  (:documentation ""))

(defvar *compilation-environment* nil
  "The active compilation environment.

Right now all code assumes that *compilation-environment* is accurately bound to the
current compilation environment--even some functions that take the compilation environment
as arguments.")

;;; parenscript packages
(defun lisp-to-script-package (lisp-package &optional (comp-env *compilation-environment*))
  "Gets a script package corresponding to the given Lisp package."
  (gethash lisp-package (comp-env-lisp-to-script-package-table comp-env)))

(defsetf lisp-to-script-package (lisp-package &optional (comp-env *compilation-environment*))
    (script-package)
  "Sets the script package corresponding to the given Lisp package."
  `(setf (gethash ,lisp-package (comp-env-lisp-to-script-package-table ,comp-env))
    ,script-package))

(defun symbol-script-package (symbol &optional (comp-env *compilation-environment*))
  "Gets the Parenscript package associated with a Lisp symbol."
  (lisp-to-script-package (symbol-package symbol) comp-env))

(defun find-script-package (name &optional (comp-env *compilation-environment*))
  "Find the script package with the name NAME in the given compilation environment."
  (find (string name) (comp-env-script-packages comp-env) :test #'equal))

(defun destroy-script-package (script-package)
  "Disposes of relevant resources when the script package is no longer relevant."
  (when (script-package-exclusive-lisp-package-p script-package)
    (delete-package (script-package-lisp-package script-package))))

;; environmental considerations
(defun make-basic-compilation-environment ()
  "Creates a compilation environment object from scratch.  Fills it in with the default
script packages (parenscript, global, and parenscript-user)."
  (let ((comp-env (make-instance 'compilation-environment)))
    comp-env))

(defun create-script-package (comp-env
			      &key name nicknames secondary-lisp-packages used-packages
			      lisp-package exports documentation)
  "Creates a script package in the given compilation environment"
  (labels ((normalize (string-like) (string string-like)))
    (let*  ((explicit-lisp-package-p (not (null lisp-package)))
	    (lisp-package
	     (or (and explicit-lisp-package-p (find-package lisp-package))
		 (make-package (gensym (string name))))))
      (labels ((package-intern (string-like)
		 (intern (normalize string-like) lisp-package)))
	(let ((script-package
	       (make-instance 'script-package
			      :name (normalize name)
			      :comp-env comp-env
			      :nicknames (mapcar #'normalize nicknames)
			      :lisp-package (find-package lisp-package)
			      :secondary-lisp-packages (mapcar #'find-package secondary-lisp-packages)
			      :exclusive-lisp-package? (not explicit-lisp-package-p)
			      :exports (mapcar #'package-intern exports)
			      :used-packages (mapcar #'(lambda (script-package-designator)
							 (find-script-package
							  script-package-designator comp-env))
						     used-packages)
			      :documentation documentation)))
	  (push script-package (comp-env-script-packages comp-env)))))))

(defmethod initialize-instance :after ((package script-package) &key)
  (assert (script-package-comp-env package))
  (assert (script-package-lisp-package package))
  (let ((lisp-packages (cons (script-package-lisp-package package)
			(script-package-secondary-lisp-packages package))))
    (dolist (lisp-package lisp-packages)
      (when (lisp-to-script-package lisp-package (script-package-comp-env package))
	(error "Lisp package already has corresponding script package: ~A" (package-name lisp-package)))
      (setf (lisp-to-script-package lisp-package (script-package-comp-env package))
	    package))))

(defgeneric comp-env-find-package (comp-env package-designator)
  (:documentation "Finds the script package named by PACKAGE-DESIGNATOR in the current 
compilation environment. PACKAGE-DESIGNATOR is a string or symbol.")
  (:method ((comp-env compilation-environment) (name string))
    (find name (comp-env-script-packages comp-env)
	  :key #'script-package-name :test #'equal))
  (:method ((comp-env compilation-environment) (package-designator symbol))
    (comp-env-find-package comp-env (string package-designator))))

;; TODO loop through all defined macros and add them to the script package's
;; macro environment
;	  (labels ((name-member (name)
;		     (eql (script-package-lisp-package script-package) (symbol-package name)))
;		   (import-macro (name function)
;		     (when (name-member name)
;		       (setf (gethash name (script-package-macro-table script-package))
;			     function)))
;		   (import-special-form (name function)
;		     (when (name-member name)
;		       (setf (gethash name (script-package-special-form-table script-package))
;			     function))))
;	  (maphash #'import-special-form *toplevel-special-forms*)
;	  (maphash #'import-special-form *toplevel-special-forms*)

;(defgeneric comp-env-select-package (comp-env script-package)
;  (:documentation "")
;  (:method ((comp-env compilation-environment) (package script-package))
;    (setf (comp-env-current-package 
  

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *toplevel-special-forms* (make-hash-table)
    "A hash-table containing functions that implement Parenscript special forms,
indexed by name (as symbols)")
  
  (defun undefine-script-special-form (name)
    "Undefines the special form with the given name (name is a symbol)."
    (declare (type symbol name))
    (when (gethash name *toplevel-special-forms*)
      (remhash name *toplevel-special-forms*))))

(defmacro define-script-special-form (name lambda-list &rest body)
  "Define a special form NAME. Arguments are destructured according to
LAMBDA-LIST. The resulting Parenscript language types are appended to the
ongoing javascript compilation."
  (declare (type symbol name))
  (let ((script-name 
	 (intern (format nil "PAREN-~A" (symbol-name name))
		 (find-package :parenscript)))
	(arglist (gensym "ps-arglist-")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defun ,script-name (&rest ,arglist)
	(destructuring-bind ,lambda-list
	    ,arglist
	  ,@body))
      (setf (gethash (quote ,name) *toplevel-special-forms*) #',script-name))))

(defun get-script-special-form (name)
  "Returns the special form function corresponding to the given name."
; (declare (type symbol name))
  (when (symbolp name)
    (gethash name *toplevel-special-forms*)))

;;; sexp form predicates
(defun script-special-form-p (form)
  "Returns T if FORM is a special form and NIL otherwise."
  (and (consp form)
       (symbolp (car form))
       (gethash (car form) *toplevel-special-forms*)))

(defun funcall-form-p (form)
  (and (listp form)
       (not (op-form-p form))
       (not (script-special-form-p form))))

(defun method-call-p (form)
  (and (funcall-form-p form)
       (symbolp (first form))
       (eql (char (symbol-name (first form)) 0) #\.)))

;;; macro expansion
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-macro-env-dictionary ()
    "Creates a standard macro dictionary."
    (make-hash-table))
  (defvar *script-macro-toplevel* (make-macro-env-dictionary)
    "Toplevel macro environment dictionary. Key is symbol-name of the macro, value
is (symbol-macro-p . expansion-function).")
  (defvar *script-macro-env* (list *script-macro-toplevel*) ;(list nil)
    "Current macro environment."))

(defmacro get-macro-spec (name env-dict)
  "Retrieves the macro spec of the given name with the given environment dictionary.
SPEC is of the form (symbol-macro-op expansion-function)."
  `(gethash ,name ,env-dict))

(defun lookup-macro-spec (name &optional (environment *script-macro-env*))
  "Looks up the macro spec associated with NAME in the given environment.  A
macro spec is of the form (symbol-macro-p function). Returns two values:
the SPEC and the parent macro environment.

NAME must be a symbol."
  (when (symbolp name)
    (do ((env environment (cdr env)))
        ((null env) nil)
      (let ((val (get-macro-spec name (car env))))
        (when val
          (return-from lookup-macro-spec
            (values val (or (cdr env)
                            (list *script-macro-toplevel*)))))))))

(defun script-symbol-macro-p (name &optional (environment *script-macro-env*))
  "True if there is a Parenscript symbol macro named by the symbol NAME."
  (and (symbolp name) (car (lookup-macro-spec name environment))))

(defun script-macro-p (name &optional (environment *script-macro-env*))
  "True if there is a Parenscript macro named by the symbol NAME."
  (and (symbolp name)
       (let ((macro-spec (lookup-macro-spec name environment)))
	 (and macro-spec (not (car macro-spec))))))

(defun lookup-macro-expansion-function (name &optional (environment *script-macro-env*))
  "Lookup NAME in the given macro expansion environment (which
defaults to the current macro environment). Returns the expansion
function and the parent macro environment of the macro."
  (multiple-value-bind (macro-spec parent-env)
      (lookup-macro-spec name environment)
    (values (cdr macro-spec) parent-env)))

(defmacro defscriptmacro (name args &body body)
  "Define a ParenScript macro, and store it in the toplevel ParenScript
macro environment."
  (let ((lambda-list (gensym "ps-lambda-list-"))
        (body (if (stringp (first body)) (rest body) body))) ;; drop docstring
    (undefine-script-special-form name)
    `(setf (get-macro-spec ',name *script-macro-toplevel*)
      (cons nil (lambda (&rest ,lambda-list)
                  (destructuring-bind ,args
                      ,lambda-list
                    ,@body))))))

(defmacro defpsmacro (name args &body body)
  `(defscriptmacro (,name ,args ,@body)))

(defun expand-script-form (expr)
  "Expands a Parenscript form down to special forms."
  (if (consp expr)
      (let ((op (car expr))
            (args (cdr expr)))
        (cond ((equal op 'quote) expr) ;; leave quotes alone
              ((script-macro-p op) ;; recursively expand parenscript macros in parent env.
	       (multiple-value-bind (expansion-function macro-env)
		   (lookup-macro-expansion-function op)
		 (expand-script-form (let ((*script-macro-env* macro-env))
				      (apply expansion-function args)))))
              (t expr)))
      ;; not a cons
      (cond ((script-special-form-p expr)
	     ;; leave special forms alone (expanded during compile)
	     expr) 
            ((script-symbol-macro-p expr)
	     ;; recursively expand symbol macros in parent env.
	     (multiple-value-bind (expansion-function macro-env)
		 (lookup-macro-expansion-function expr)
	       (expand-script-form (let ((*script-macro-env* macro-env))
				    (funcall expansion-function)))))
	    ;; leave anything else alone
            (t expr))))

;;;; compiler interface ;;;;
(defgeneric compile-parenscript-form (compilation-environment form)
  (:documentation "Compiles FORM, which is a ParenScript form, into a pre-text
compilation object (the AST root).  Subsequently TRANSLATE-AST can be called
to convert the result to Javascript."))

(defmethod compile-parenscript-form ((comp-env compilation-environment) form)
  (setf form (expand-script-form form))
  (cond ((stringp form)
	 (make-instance 'string-literal :value form))
        ((characterp form)
	 (make-instance 'string-literal :value (string form)))
	((numberp form)
	 (make-instance 'number-literal :value form))
	((symbolp form) ;; is this the correct behavior?
	 (let ((c-macro (get-script-special-form form)))
	   (if c-macro
	       (funcall c-macro)
	       (make-instance 'script-variable :value form))))
	((and (consp form)
	      (eql (first form) 'quote))
	 (make-instance 'script-quote :value (second form)))
	((consp form)
	 (let* ((name (car form))
		(args (cdr form))
		(script-form (get-script-special-form name)))
	   (cond (script-form
		  (apply script-form args))
		 
		 ((op-form-p form)
		  (make-instance 'op-form
				 :operator (script-convert-op-name (compile-to-symbol (first form)))
				 :args (mapcar #'compile-to-expression (rest form))))
		 
		 ((method-call-p form)
		  (make-instance 'method-call
				 :method (compile-to-symbol (first form))
				 :object (compile-to-expression (second form))
				 :args (mapcar #'compile-to-expression (cddr form))))
		 
		 ((funcall-form-p form)
		  (make-instance 'function-call
				 :function (compile-to-expression (first form))
				 :args (mapcar #'compile-to-expression (rest form))))
		 
		 (t (error "Unknown form ~S" form)))))
	(t (error "Unknown atomar expression ~S" form))))

(defun compile-script-form (form &key (comp-env *compilation-environment*))
  "Compiles a Parenscript form to an AST node."
  (compile-parenscript-form *compilation-environment* form ))

(defun compile-to-expression (form)
  "Compiles the given Parenscript form and guarantees the result is an expression."
  (let ((res (compile-script-form form)))
    (assert (typep res 'expression))
    res))

(defun compile-to-symbol (form)
  "Compiles the given Parenscript form and guarantees a symbolic result."
  (let ((res (compile-script-form form)))
    (when (typep res 'script-variable)
      (setf res (value res)))
    (assert (symbolp res) ()
            "~a is expected to be a symbol, but compiles to ~a. This could be due to ~a being a special form." form res form)
    res))

(defun compile-to-statement (form)
  "Compiles the given Parenscript form and guarantees the result is a statement."
  (let ((res (compile-script-form form)))
    (assert (typep res 'statement))
    res))

(defun compile-to-body (form &key (indent ""))
  "Compiles the given Parenscript form and guarantees the result is of type SCRIPT-BODY"
  (let ((res (compile-to-statement form)))
    (if (typep res 'script-body)
	(progn (setf (b-indent res) indent)
	       res)
	(make-instance 'script-body
		       :indent indent
		       :statements (list res)))))