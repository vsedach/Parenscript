(in-package :parenscript)

;;;; The mechanisms for defining macros & parsing Parenscript.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *enable-package-system* t
    "When NIL, all symbols will function as global symbols."))
  
(eval-when (:compile-toplevel :load-toplevel)
  (defun macro-name-hash-function ()
    (if *enable-package-system* #'eql #'equal)))

(defclass script-package ()
  ;; configuration slots
  ((name          :accessor script-package-name          :initform nil :initarg :name :type string
		  :documentation "Canonical name of the package (a String).")
   (nicknames     :accessor script-package-nicknames     :initform nil :initarg :nicknames
		  :documentation "List of nicknames for the package (as strings).")
   (lisp-package  :accessor script-package-lisp-package  :initform nil :initarg :lisp-package)
   (secondary-lisp-packages :accessor script-package-secondary-lisp-packages :initform nil
			    :initarg :secondary-lisp-packages)
   (exports       :accessor script-package-exports       :initarg :exports
		  :initform nil;(make-hash-table :test #'equal)
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
;   (toplevel-identifiers :accessor script-package-toplevel-ids :initarg :toplevel-ids
;                         :initform nil)
;   (macro-table   :accessor script-package-macro-table
;                  :initform (make-hash-table :test #'eql)
;                  :documentation "This package's macro environment, set up as a hash table
;                                  from symbols to macro functions")
;   (special-form-table :accessor script-package-special-form-table
;		       :initform (make-hash-table :test #'equal)
;		       :documentation "Holds special form macros for the package.
;                                       Probably not used except for built-in packages."))
   )
  (:documentation "A Parenscript package is a lisp object that holds information
about a set of code.

"))

(defclass compilation-environment ()
  ((script-packages :accessor comp-env-script-packages :initform nil :initarg :packages
		    :documentation "List of packages defined in this environment.")
   (current-package :accessor comp-env-current-package :initform nil :initarg :current-package
		    :documentation "Current in-package.")

   (lisp-to-script-package-table
    :accessor comp-env-lisp-to-script-package-table :initform (make-hash-table)
    :documentation   "Maps a lisp package to a script package.")
   (compiling-toplevel-p 
    :accessor comp-env-compiling-toplevel-p :initform nil :initarg :processing-toplevel-p
    :documentation "T if the environment is currently processing toplevel forms."))
  (:documentation ""))

(defgeneric compiler-in-situation-p (comp-env situation)
  (:documentation "Returns true when the compiler is considered 'in' the situation
given by SITUATION, which is one of :compile-toplevel :execute.")
  (:method ((comp-env compilation-environment) situation)
    (cond
      ((eql situation :compile-toplevel) (processing-toplevel-p comp-env))
      ((eql situation :execute) (not (processing-toplevel-p comp-env)))
      (t nil))))

(defgeneric processing-toplevel-p (comp-env)
  (:documentation "T if we are compiling TOPLEVEL forms, as in 
http://www.lispworks.com/documentation/HyperSpec/Body/03_bca.htm")
  (:method ((comp-env compilation-environment))
    (comp-env-compiling-toplevel-p comp-env)
    ))

(defvar *compilation-environment* nil
  "The active compilation environment."
;; Right now all code assumes that *compilation-environment* is accurately bound to the
;; current compilation environment--even some functions that take the compilation environment
;; as arguments.
  )

(defvar *package-prefix-style* :prefix
  "Determines how package symbols are serialized to JavaScript identifiers.  NIL for
no prefixes.  :prefix to prefix variables with something like packagename_identifier.")

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
  (typecase name
    ((or symbol string)
     (find-if #'(lambda (script-package)
		  (find (string name)
			(cons (script-package-name script-package)
			      (script-package-nicknames script-package))
			:test #'equal))
	      (comp-env-script-packages comp-env)))
    (script-package  name)
    (t (error "~A has unknown type" name))))
     
(defun destroy-script-package (script-package)
  "Disposes of relevant resources when the script package is no longer relevant."
  (when (script-package-exclusive-lisp-package-p script-package)
    (delete-package (script-package-lisp-package script-package))))

(defun script-intern (name script-package)
  "Returns a Parenscript symbol with the string value STRING interned for the
given SCRIPT-PACKAGE."
  (setf script-package (find-script-package script-package))
  (flet ((find-exported-symbol (name script-package)
	   (let ((res
		  (find name (script-package-exports script-package)
			:key #'(lambda (exported-symbol) (string exported-symbol))
			:test #'equal)))
;	     (format t "Searching for exported symbol ~A in ~A: ~A~%" 
;		     name (script-package-name script-package) res)
	     res)))
    (let ((res
	   (or
	    (some #'(lambda (used-package)
		      (find-exported-symbol name used-package))
		  (script-package-used-packages script-package))
	    (intern name (script-package-lisp-package script-package)))))
      (declare (type symbol res))
      res)))


(defun find-script-symbol (name script-package)
  "Finds the symbol with name NAME in the script package SCRIPT-PACKAGE.  NAME is a
string and SCRIPT-PACKAGE is a package designator.  If NAME does not specify a symbol of
script-package, returns nil.  Otherwise returns 2 values:
1.  the symbol
2.  :external if the symbol is external.  :internal if the symbol is internal"
  (setf script-package (find-script-package script-package))
  (let* ((symbol (find-symbol name (script-package-lisp-package script-package)))
         (exported? (find symbol (script-package-exports script-package))))
    (values symbol (if exported? :external (when symbol :internal)))))

(defun script-export (symbols
		      &optional (script-package (comp-env-current-package *compilation-environment*)))
  "Exports the given symbols in the given script package."
  (when (not (listp symbols)) (setf symbols (list symbols)))
  (setf script-package (find-script-package script-package))
;  (format t "Exporting symbols ~A in package ~A~%"
;	  symbols (script-package-name script-package))
  (let ((symbols-not-in-package
	 (remove-if #'(lambda (symbol)
			(declare (type symbol symbol))
			(eql symbol (find-script-symbol (string symbol) script-package)))
		    symbols)))
    (when symbols-not-in-package
      (error "Invalid exports.  The following symbols are not interned in the package ~A:~%~A"
	     (script-package-name script-package) symbols-not-in-package)))
  (mapc #'(lambda (symbol)
	    (pushnew symbol (script-package-exports script-package)))
	symbols)
  t)
  
(defun use-script-package (packages-to-use
			   &optional (into-package (comp-env-current-package *compilation-environment*)))
  "use-script-package causes INTO-PACKAGE to inherit all the external symbols of packages-to-use. 
The inherited symbols become accessible as internal symbols of package."
  (when (not (listp packages-to-use)) (setf packages-to-use (list packages-to-use)))
  (setf packages-to-use (mapcar #'find-script-package packages-to-use))
  (setf into-package (find-script-package into-package))

  (let ((all-used-symbols (apply #'append (mapcar #'script-package-exports packages-to-use))))
    (mapc #'(lambda (used-symbol)
	      (let ((symbol-same-name (find-script-symbol (string used-symbol) into-package)))
		(when (not (or (null symbol-same-name)
			       (eql symbol-same-name used-symbol)))
		  (error "Import of symbol ~A into package ~A conflicts with interned symbol ~A"
			 used-symbol (script-package-name into-package) symbol-same-name))))
	  all-used-symbols))
  (setf (script-package-used-packages into-package)
	(append (script-package-used-packages into-package) packages-to-use)))
	


;; environmental considerations
(defgeneric setup-compilation-environment (comp-env)
  (:documentation "Sets up a basic compilation environment prepared for a language user.
This should do things like define packages and set the current package.

Returns the compilation-environment."))

(defgeneric install-standard-script-packages (comp-env)
  (:documentation "Creates standard script packages and installs them in the current compilation
environment."))

(defun make-basic-compilation-environment ()
  "Creates a compilation environment object from scratch.  Fills it in with the default
script packages (parenscript, global, and parenscript-user)."
  (let ((*compilation-environment* (make-instance 'compilation-environment)))
    (setup-compilation-environment *compilation-environment*)))

(defun create-script-package (comp-env
			      &key name nicknames secondary-lisp-packages used-packages
			      lisp-package exports documentation)
  "Creates a script package in the given compilation environment"
  (let*  ((explicit-lisp-package-p (not (null lisp-package)))
	  (lisp-package
	   (or (and explicit-lisp-package-p (find-package lisp-package))
	       (make-package (gensym (string name))))))
    (let ((script-package
	   (make-instance 'script-package
			  :name (string name)
			  :comp-env comp-env
			  :nicknames (mapcar #'string nicknames)
			  :lisp-package (find-package lisp-package)
			  :secondary-lisp-packages (mapcar #'find-package secondary-lisp-packages)
			  :exclusive-lisp-package? (not explicit-lisp-package-p)
			  :documentation documentation)))
	(use-script-package used-packages script-package)
;	(format t "CSP exports for ~A: ~A~%" (script-package-name script-package) exports)
	(labels ((package-intern (string-like)
		   (script-intern (string string-like) script-package)))
	  (script-export (mapcar #'package-intern exports) script-package))
	(push script-package (comp-env-script-packages comp-env))
	script-package)))

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
  (defvar *toplevel-special-forms* (make-hash-table :test (macro-name-hash-function))
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
    `(setf (gethash (quote ,name) *toplevel-special-forms*)
      #'(lambda (&rest ,arglist)
	  (destructuring-bind ,lambda-list
	      ,arglist
	    ,@body)))))
	   

(defun get-script-special-form (name)
  "Returns the special form function corresponding to the given name."
; (declare (type symbol name))
  (cond
    (*enable-package-system*
     (when (symbolp name)
       (gethash name *toplevel-special-forms*)))
    (t
     (when (symbolp name)
       (maphash #'(lambda (macro-name value)
		    (when (equal (string macro-name) (string name))
		      (return-from get-script-special-form value)))
		*toplevel-special-forms*)))))

;;; sexp form predicates
(defun script-special-form-p (form)
  "Returns T if FORM is a special form and NIL otherwise."
  (and (consp form)
       (symbolp (car form))
       (get-script-special-form (car form))))

(defun funcall-form-p (form)
  (and (listp form)
       (not (ps-js::op-form-p form))
       (not (script-special-form-p form))))

(defun method-call-p (form)
  (and (funcall-form-p form)
       (symbolp (first form))
       (eql (char (symbol-name (first form)) 0) #\.)))

;;; macro expansion
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-macro-env-dictionary ()
    "Creates a standard macro dictionary."
    (make-hash-table  :test (macro-name-hash-function)))
  (defvar *script-macro-toplevel* (make-macro-env-dictionary)
    "Toplevel macro environment dictionary. Key is symbol-name of the macro, value
is (symbol-macro-p . expansion-function).")
  (defvar *script-macro-env* (list *script-macro-toplevel*) ;(list nil)
    "Current macro environment.")
  
  (defun find-macro-spec (name env-dict)
    (if *enable-package-system*
	(gethash name env-dict)
	(with-hash-table-iterator (next-entry env-dict)
	  (loop
	   (multiple-value-bind (exists? macro-name spec)
	       (next-entry)
	     (if exists?
		 (when (equal (string macro-name) (string name))
		   (return spec))
		 (return nil)))))))
  (defsetf find-macro-spec (name env-dict)
      (spec)
    `(setf (gethash ,name ,env-dict) ,spec)))


(defmacro get-macro-spec (name env-dict)
  "Retrieves the macro spec of the given name with the given environment dictionary.
SPEC is of the form (symbol-macro-op expansion-function)."
  `(find-macro-spec ,name ,env-dict))

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

(defun process-eval-when-args (args)
  "(eval-when form-language? (situation*) form*) - returns 3 values: 
form-language, a list of situations, and a list of body forms"
  (let* ((rest args)
	 (form-language
	  (when (not (listp (first rest)))
	    (setf rest (rest args))
	    (first args)))
	 (situations (first rest))
	 (body (rest rest)))
    (when (and (find :compile-toplevel situations) (find :execute situations))
      (error "Cannot use EVAL-WHEN to execute COMPILE-TOPLEVEL and EXECUTE code simultaneously."))
    (when (null form-language)
      (setf form-language
	    (cond
	      ((find :compile-toplevel situations) :lisp)
	      ((find :execute situations)          :parenscript))))
    (values form-language situations body)))
  
;;;; compiler interface ;;;;
(defgeneric compile-parenscript-form (compilation-environment form &key toplevel-p)
  (:documentation "Compiles FORM, which is a ParenScript form.
If toplevel-p is NIL, the result is a compilation object (the AST root).
Subsequently TRANSLATE-AST can be called to convert the result to Javascript.

If the compiler is in the COMPILE-TOPLEVEL stage, then the result will
be a Parenscript form (after it has been processed according to semantics
like those of Lisp's COMPILE-FILE). See
http://www.lispworks.com/documentation/HyperSpec/Body/03_bca.htm"))

(defmethod compile-parenscript-form ((comp-env compilation-environment) form &key toplevel-p)
  (setf form (expand-script-form form))
  ;; ensures proper compilation environment TOPLEVEL-P slot value
  (setf (comp-env-compiling-toplevel-p comp-env) toplevel-p)
  (if
   toplevel-p
   (cond
     ((not (listp form)) form)
     ;; process each clause of a progn as a toplevel form
     ((eql 'progn (car form))
      `(progn
	,@(mapcar #'(lambda (subform)
		      (compile-parenscript-form comp-env subform :toplevel-p t))
		  (rest form))))
     ;; TODO process macrolets, symbol-macrolets, and file inclusions
     
     ;; process eval-when.  evaluates in :COMPILE-TOPLEVEL situation and returns
     ;; the resultant form.  for :EXECUTE situation it returns 
     ((eql 'eval-when (car form))
      (multiple-value-bind (body-language situations body)
	  (process-eval-when-args (rest form))
	(cond
	  ((find :compile-toplevel situations)
	   (when (eql body-language :lisp)
	     (let ((other-situations (remove :compile-toplevel situations)))
	       (multiple-value-bind (function warnings-p failure-p)
		   (compile nil `(lambda () ,@body))
		 (declare (ignore warnings-p) (ignore failure-p))
		 (compile-parenscript-form 
		  comp-env
		  `(progn
		    ,(funcall function)
		    ,@(when other-situations
			    (list `(eval-when ,other-situations ,@body))))
		  :toplevel-p t)))))
	  ;; if :compile-toplevel is not in the situation list, return the form
	  (t form))))
     (t form))
   (cond ((stringp form)
	  (make-instance 'ps-js::string-literal :value form))
	 ((characterp form)
	  (make-instance 'ps-js::string-literal :value (string form)))
	 ((numberp form)
	  (make-instance 'ps-js::number-literal :value form))
	 ((symbolp form)
	  ;; is this the correct behavior?
	  (let ((c-macro (get-script-special-form form)))
	    (if c-macro
		(funcall c-macro)
		(make-instance 'ps-js::js-variable :value form))))
	 ((and (consp form)
	       (eql (first form) 'quote))
	  (make-instance 'script-quote :value (second form)))
	 ((consp form)
	  (let* ((name (car form))
		 (args (cdr form))
		 (script-form (get-script-special-form name)))
	       (cond (script-form
		      (apply script-form args))
		     
		     ((ps-js::op-form-p form)
		      (make-instance 'ps-js::op-form
				     :operator (ps-js::script-convert-op-name (compile-to-symbol (first form)))
				     :args (mapcar #'compile-to-expression (rest form))))
		     
		     ((method-call-p form)
		      (make-instance 'ps-js::method-call
				     :method (compile-to-symbol (first form))
				     :object (compile-to-expression (second form))
				     :args (mapcar #'compile-to-expression (cddr form))))
		     
		     ((funcall-form-p form)
		      (make-instance 'ps-js::function-call
				     :function (compile-to-expression (first form))
				     :args (mapcar #'compile-to-expression (rest form))))
		     
		     (t (error "Unknown form ~S" form)))))
	 (t (error "Unknown atomar expression ~S" form)))))

(defun compile-script-form (form &key (comp-env *compilation-environment*))
  "Compiles a Parenscript form to an AST node."
  (compile-parenscript-form comp-env form ))

(defun compile-to-expression (form)
  "Compiles the given Parenscript form and guarantees the result is an expression."
  (let ((res (compile-script-form form)))
    (assert (typep res 'ps-js::expression))
    res))

(defun compile-to-symbol (form)
  "Compiles the given Parenscript form and guarantees a symbolic result.  This
also guarantees that the symbol has an associated script-package."
  (let ((res (compile-script-form form)))
    (when (typep res 'ps-js::js-variable)
      (setf res (ps-js::value res)))
    (assert (symbolp res) ()
            "~a is expected to be a symbol, but compiles to ~a. This could be due to ~a being a special form." form res form)
    (when *enable-package-system*
      (assert (symbol-script-package res) ()
	      "The symbol ~A::~A has no associated script package." 
	      (package-name (symbol-package res))
	      res))
    res))

(defun compile-to-statement (form)
  "Compiles the given Parenscript form and guarantees the result is a statement."
  (let ((res (compile-script-form form)))
    (assert (typep res 'ps-js::statement))
    res))

(defun compile-to-block (form &key (indent ""))
  "Compiles the given Parenscript form and guarantees the result is of type SCRIPT-BODY"
  (let ((res (compile-to-statement form)))
    (if (typep res 'ps-js::js-block)
	(progn (setf (ps-js::block-indent res) indent)
	       res)
	(make-instance 'ps-js::js-block
		       :indent indent
		       :statements (list res)))))