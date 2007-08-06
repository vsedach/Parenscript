(in-package :parenscript)

;;;; The mechanisms for defining macros & parsing Parenscript.
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *toplevel-special-forms* (make-hash-table :test #'equal)
    "A hash-table containing functions that implement Parenscript special forms,
indexed by name (as symbols)")
  (defun undefine-script-special-form (name)
    "Undefines the special form with the given name (name is a symbol)."
    (remhash (lisp-symbol-to-ps-identifier name :special-form) *toplevel-special-forms*)))

(defmacro define-script-special-form (name lambda-list &rest body)
  "Define a special form NAME. Arguments are destructured according to
LAMBDA-LIST. The resulting Parenscript language types are appended to the
ongoing javascript compilation."
  (let ((arglist (gensym "ps-arglist-")))
    `(setf (gethash (lisp-symbol-to-ps-identifier ',name :special-form) *toplevel-special-forms*)
      (lambda (&rest ,arglist)
        (destructuring-bind ,lambda-list
            ,arglist
          ,@body)))))

(defun get-script-special-form (name)
  "Returns the special form function corresponding to the given name."
  (gethash (lisp-symbol-to-ps-identifier name :special-form) *toplevel-special-forms*))

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
    (make-hash-table :test #'equal))
  (defvar *script-macro-toplevel* (make-macro-env-dictionary)
    "Toplevel macro environment dictionary. Key is the symbol of the
macro, value is (symbol-macro-p . expansion-function).")
  (defvar *script-macro-env* (list *script-macro-toplevel*)
    "Current macro environment.")

  (defvar *script-setf-expanders* (make-macro-env-dictionary)
    "Setf expander dictionary. Key is the symbol of the access
function of the place, value is an expansion function that takes the
arguments of the access functions as a first value and the form to be
stored as the second value.")
  
  (defun get-macro-spec (name env-dict)
    "Retrieves the macro spec of the given name with the given environment dictionary.
SPEC is of the form (symbol-macro-p . expansion-function)."
    (gethash (lisp-symbol-to-ps-identifier name :macro) env-dict))
  (defsetf get-macro-spec (name env-dict)
      (spec)
    `(setf (gethash (lisp-symbol-to-ps-identifier ,name :macro) ,env-dict) ,spec)))

(defun lookup-macro-spec (name &optional (environment *script-macro-env*))
  "Looks up the macro spec associated with NAME in the given environment.  A
macro spec is of the form (symbol-macro-p . function). Returns two values:
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-script-macro% (name args body &key symbol-macro-p)
    (let ((lambda-list (gensym "ps-lambda-list-"))
	  (body (if (and (cdr body) (stringp (first body))) (rest body) body))) ;; drop docstring
      (undefine-script-special-form name)
      (setf (get-macro-spec name *script-macro-toplevel*)
	    (cons symbol-macro-p (compile nil `(lambda (&rest ,lambda-list)
						(destructuring-bind ,args
						    ,lambda-list
						  ,@body)))))
      nil)))

(defmacro defscriptmacro (name args &body body)
  "Define a ParenScript macro, and store it in the toplevel ParenScript
macro environment."
  `(define-script-macro% ',name ',args ',body :symbol-macro-p nil))

(defmacro define-script-symbol-macro (name &body body)
  "Define a ParenScript symbol macro, and store it in the toplevel ParenScript
macro environment.  BODY is a Lisp form that should return a ParenScript form."
  `(define-script-macro% ',name () ',body :symbol-macro-p t))

(defun import-macros-from-lisp (&rest names)
  "Import the named Lisp macros into the ParenScript macro
environment. When the imported macro is macroexpanded by ParenScript,
it is first fully macroexpanded in the Lisp macro environment, and
then that expansion is further expanded by ParenScript."
  (dolist (name names)
    (define-script-macro% name '(&rest args) 
      (list `(common-lisp:macroexpand `(,',name ,@args)))
      :symbol-macro-p nil)))

(defmacro defmacro/ps (name args &body body)
  "Define a Lisp macro and import it into the ParenScript macro environment."
  `(progn (defmacro ,name ,args ,@body)
	  (ps:import-macros-from-lisp ',name)))

(defmacro defmacro+ps (name args &body body)
  "Define a Lisp macro and a ParenScript macro in their respective
macro environments. This function should be used when you want to use
the same macro in both Lisp and ParenScript, but the 'macroexpand' of
that macro in Lisp makes the Lisp macro unsuitable to be imported into
the ParenScript macro environment."
  `(progn (defmacro ,name ,args ,@body)
          (defscriptmacro ,name ,args ,@body)))

(defmacro defpsmacro (&rest args)
  `(defscriptmacro ,@args))

(defun expand-script-form (expr)
  "Expands a Parenscript form until it reaches a special form.  Returns 2 values:
1. the expanded form.
2. whether the form was expanded."
  (if (consp expr)
      (let ((op (car expr))
            (args (cdr expr)))
        (cond ((equal op 'quote)
	       (values 
		(if (equalp '(nil) args) nil expr) ;; leave quotes alone, unless it's a quoted nil
		nil))
              ((script-macro-p op) ;; recursively expand parenscript macros in parent env.
	       (multiple-value-bind (expansion-function macro-env)
		   (lookup-macro-expansion-function op)
		 (values
		  (expand-script-form (let ((*script-macro-env* macro-env))
					(apply expansion-function args)))
		  t)))
              ((script-special-form-p expr)
               (values expr nil))
              (t (values expr nil))))
      (cond ((script-symbol-macro-p expr)
	     ;; recursively expand symbol macros in parent env.
	     (multiple-value-bind (expansion-function macro-env)
		 (lookup-macro-expansion-function expr)
	       (values
		(expand-script-form (let ((*script-macro-env* macro-env))
				      (funcall expansion-function)))
		t)))
	    ;; leave anything else alone
            (t (values expr nil)))))

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
(defgeneric compile-parenscript-form (compilation-environment form)
  (:documentation "Compiles FORM, which is a ParenScript form.
If toplevel-p is NIL, the result is a compilation object (the AST root).
Subsequently TRANSLATE-AST can be called to convert the result to Javascript.

If the compiler is in the COMPILE-TOPLEVEL stage, then the result will
be a Parenscript form (after it has been processed according to semantics
like those of Lisp's COMPILE-FILE). See
http://www.lispworks.com/documentation/HyperSpec/Body/03_bca.htm"))

(defgeneric compile-toplevel-parenscript-form (comp-env form)
  (:documentation "Compiles a parenscript form in the given compilation environment
when the environment is in the :compile-toplevel situation.  Returns a form to be
compiled in place of the original form upon exiting the :compile-toplevel situation."))

(defmethod compile-toplevel-parenscript-form ((comp-env compilation-environment) form)
  (cond
    ((not (listp form)) form)
    ;; process each clause of a progn as a toplevel form
    ((eql 'progn (car form))
     `(progn
       ,@(mapcar #'(lambda (subform)
		     (compile-parenscript-form comp-env subform))
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
			   (list `(eval-when ,other-situations ,@body)))))))))
	 ;; if :compile-toplevel is not in the situation list, return the form
	 (t form))))
    (t form)))


(defmethod compile-parenscript-form :around ((comp-env compilation-environment) form)
  (multiple-value-bind (expanded-form expanded-p)
      (expand-script-form form)
    (cond
      (expanded-p
       (compile-parenscript-form comp-env expanded-form))
      ((comp-env-compiling-toplevel-p comp-env)
       (compile-toplevel-parenscript-form comp-env form))
      (t (call-next-method)))))

(defmethod compile-parenscript-form ((comp-env compilation-environment) (form string))
  (make-instance 'ps-js::string-literal :value form))

(defmethod compile-parenscript-form ((comp-env compilation-environment) (form character))
  (compile-parenscript-form comp-env (string form)))

(defmethod compile-parenscript-form ((comp-env compilation-environment) (form number))
  (make-instance 'ps-js::number-literal :value form))

(defmethod compile-parenscript-form ((comp-env compilation-environment) (form symbol))
  ;; is this the correct behavior?
  (let ((c-macro (get-script-special-form form)))
    (cond
      (c-macro	(funcall c-macro))
      ;; the following emulates the lisp behavior that a keyword is bound to itself
      ;; see http://clhs.lisp.se/Body/t_kwd.htm
      ((keywordp form) (compile-parenscript-form comp-env `(quote ,form)))
      (t (make-instance 'ps-js::js-variable :value form)))))

(defun compile-function-argument-forms (forms)
  "Compiles a bunch of Parenscript forms from a funcall form to an effective set of
Javascript arguments.  The only extra processing this does is makes :keyword arguments
into a single options argument via CREATE."
  (flet ((keyword-arg (arg)
	   "If the given compiled expression is supposed to be a keyword argument, returns
the keyword for it."
	   (when (typep arg 'script-quote) (ps-js::value arg))))
  (let ((expressions (mapcar #'compile-to-expression forms)))

    (do ((effective-expressions nil)
	 (expressions-subl expressions))

	((not expressions-subl)
	 (nreverse effective-expressions))
      
      (let ((arg-expr (first expressions-subl)))
	(if (keyword-arg arg-expr)
	    (progn
	      (when (oddp (length expressions-subl))
		(error "Odd number of keyword arguments: ~A." forms))
	      (push
	       (make-instance 'ps-js::js-object
			      :slots
			      (loop for (name val) on expressions-subl by #'cddr
				    collect (list name val)))
	       effective-expressions)
	      (setf expressions-subl nil))
	    (progn
	      (push arg-expr effective-expressions)
	      (setf expressions-subl (rest expressions-subl)))))))))

(defmethod compile-parenscript-form ((comp-env compilation-environment) (form cons))
  (let* ((name (car form))
	 (args (cdr form))
	 (script-form (when (symbolp name) (get-script-special-form name))))
    (cond
      ((eql name 'quote)       (make-instance 'script-quote :value (first args)))
      (script-form             (apply script-form args))
      ((ps-js::op-form-p form)
       (make-instance 'ps-js::op-form
		      :operator (ps-js::script-convert-op-name (compile-to-symbol (first form)))
		      :args (mapcar #'compile-to-expression (rest form))))
      ((method-call-p form)
       (make-instance 'ps-js::method-call
		      :method (compile-to-symbol name)
		      :object (compile-to-expression (first args))
		      :args (compile-function-argument-forms (rest args))))
      ((funcall-form-p form)
       (make-instance 'ps-js::function-call
		      :function (compile-to-expression name)
		      :args (compile-function-argument-forms args)))
      (t (error "Unknown form ~S" form)))))

(defun compile-script-form (form &key (comp-env *compilation-environment*))
  "Compiles a Parenscript form to an AST node."
  (compile-parenscript-form comp-env form))

(defun compile-to-expression (form)
  "Compiles the given Parenscript form and guarantees the result is an expression."
  (let ((res (compile-script-form form)))
    (assert (typep res 'ps-js::expression) ()
            "Error: ~s was expected to compile to a ParenScript expression, but instead compiled to ~s, which has type ~s"
            form res (type-of res))
    res))

(defun compile-to-symbol (form)
  "Compiles the given Parenscript form and guarantees a symbolic result.  This
also guarantees that the symbol has an associated script-package."
  (let ((res (compile-script-form form)))
    (when (typep res 'ps-js::js-variable)
      (setf res (ps-js::value res)))
    (when (typep res 'ps-js::script-quote)
      (setf res (ps-js::value res)))
    (assert (symbolp res) ()
            "~a is expected to be a symbol, but compiles to ~a (the ParenScript output for ~a alone is \"~a\"). This could be due to ~a being a special form." form res form (ps::ps* form) form)
    (unless (symbol-script-package res)
      (when *warn-ps-package*
        (warn 'simple-style-warning
              :format-control "The symbol ~A::~A has no associated script package."
              :format-arguments (list (if (symbol-package res) (package-name (symbol-package res)) "ANONYMOUS-PACKAGE")
                                      res))))
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