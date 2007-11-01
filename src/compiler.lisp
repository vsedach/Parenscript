(in-package :parenscript)

;;;; The mechanisms for parsing Parenscript.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *toplevel-special-forms* (make-hash-table :test #'equal)
    "A hash-table containing functions that implement Parenscript special forms,
indexed by name (as symbols)")
  (defun undefine-ps-special-form (name)
    "Undefines the special form with the given name (name is a symbol)."
    (remhash (lisp-symbol-to-ps-identifier name :special-form) *toplevel-special-forms*)))

(defmacro define-ps-special-form (name lambda-list &rest body)
  "Define a special form NAME. The first argument given to the special
form is a keyword indicating whether the form is expected to produce
an :expression or a :statement. The resulting Parenscript language
types are appended to the ongoing javascript compilation."
  (let ((arglist (gensym "ps-arglist-")))
    `(setf (gethash (lisp-symbol-to-ps-identifier ',name :special-form) *toplevel-special-forms*)
      (lambda (&rest ,arglist)
        (destructuring-bind ,lambda-list
            ,arglist
          ,@body)))))

(defun get-ps-special-form (name)
  "Returns the special form function corresponding to the given name."
  (gethash (lisp-symbol-to-ps-identifier name :special-form) *toplevel-special-forms*))

;;; ParenScript form predicates
(defun ps-special-form-p (form)
  (and (consp form)
       (symbolp (car form))
       (get-ps-special-form (car form))))

(defun op-form-p (form)
  (and (listp form)
       (not (ps-special-form-p form))
       (not (null (op-precedence (first form))))))

(defun funcall-form-p (form)
  (and (listp form)
       (not (op-form-p form))
       (not (ps-special-form-p form))))

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
  (defun make-ps-macro-function (args body)
    (let* ((whole-var (when (eql '&whole (first args)) (second args)))
           (effective-lambda-list (if whole-var (cddr args) args))
           (form-arg (or whole-var (gensym "ps-macro-form-arg-")))
           (body (if (and (cdr body) (stringp (first body))) (rest body) body))) ;; drop docstring
      (compile nil `(lambda (,form-arg)
                     (destructuring-bind ,effective-lambda-list
                         (cdr ,form-arg)
                       ,@body)))))
      
  (defun define-script-macro% (name args body &key symbol-macro-p)
    (undefine-ps-special-form name)
    (setf (get-macro-spec name *script-macro-toplevel*)
          (cons symbol-macro-p (make-ps-macro-function args body)))
    nil))

(defmacro defpsmacro (name args &body body)
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
          (defpsmacro ,name ,args ,@body)))

(defun ps-macroexpand (form)
  "Recursively macroexpands ParenScript macros and symbol-macros in
the given ParenScript form. Returns two values: the expanded form, and
whether any expansion was performed on the form or not."
  (if (consp form)
      (let ((op (car form))
            (args (cdr form)))
        (cond ((equal op 'quote) (values (if (equalp '(nil) args) nil form) ;; leave quotes alone, unless it's a quoted nil
                                         nil))
              ((script-macro-p op) (values (ps-macroexpand (funcall (lookup-macro-expansion-function op) form)) t))
              (t (values form nil))))
      (cond ((script-symbol-macro-p form) (values (ps-macroexpand (funcall (lookup-macro-expansion-function form) (list form))) t))
            (t (values form nil)))))

;;;; compiler interface
(defgeneric compile-parenscript-form (form &key expecting)
  (:documentation "Compiles a ParenScript form to the intermediate
ParenScript representation. :expecting determines whether the form is
compiled to an :expression (the default), a :statement, or a
:symbol."))

(defmethod compile-parenscript-form :around (form &key expecting)
  (if (eql expecting :symbol)
      (compile-to-symbol form)
      (multiple-value-bind (expanded-form expanded-p)
          (ps-macroexpand form)
        (if expanded-p
            (compile-parenscript-form expanded-form :expecting expecting)
            (call-next-method)))))

(defun compile-to-symbol (form)
  "Compiles the given Parenscript form and guarantees that the
resultant symbol has an associated script-package. Raises an error if
the form cannot be compiled to a symbol."
  (let ((exp (compile-parenscript-form form)))
    (when (or (eql (first exp) 'js-variable)
              (eql (first exp) 'script-quote))
      (setf exp (second exp)))
    (assert (symbolp exp) ()
            "~a is expected to be a symbol, but compiles to ~a (the ParenScript output for ~a alone is \"~a\"). This could be due to ~a being a special form." form exp form (ps* form) form)
    exp))

(defmethod compile-parenscript-form (form &key expecting)
  (declare (ignore expecting))
  (error "The object ~S cannot be compiled by ParenScript." form))

(defmethod compile-parenscript-form ((form number) &key expecting)
  (declare (ignore expecting))
  form)

(defmethod compile-parenscript-form ((form string) &key expecting)
  (declare (ignore expecting))
  form)

(defmethod compile-parenscript-form ((form character) &key expecting)
  (declare (ignore expecting))
  (compile-parenscript-form (string form)))

(defmethod compile-parenscript-form ((symbol symbol) &key expecting)
  (declare (ignore expecting))
  ;; is this the correct behavior?
  (let ((special-symbol (get-ps-special-form symbol)))
    (cond (special-symbol (funcall special-symbol :symbol))
          (t (list 'js-variable symbol)))))

(defun compile-function-argument-forms (arg-forms)
  "Compiles a bunch of Parenscript forms from a funcall form to an effective set of
Javascript arguments.  The only extra processing this does is makes :keyword arguments
into a single options argument via CREATE."
  (flet ((keyword-arg (arg)
	   "If the given compiled expression is supposed to be a keyword argument, returns
the keyword for it."
	   (when (and (listp arg) (eql (first arg) 'script-quote)) (second arg))))
    (let ((compiled-args (mapcar (lambda (arg) (compile-parenscript-form arg :expecting :expression))
                                 arg-forms)))
      (do ((effective-expressions nil)
           (expressions-subl compiled-args))
          ((not expressions-subl) (reverse effective-expressions))
        (let ((arg-expr (first expressions-subl)))
          (if (keyword-arg arg-expr)
              (progn (when (oddp (length expressions-subl))
                       (error "Odd number of keyword arguments: ~A." arg-forms))
                     (push (list 'js-object (loop for (name val) on expressions-subl by #'cddr
                                                  collect (list name val)))
                           effective-expressions)
                     (setf expressions-subl nil))
              (progn (push arg-expr effective-expressions)
                     (setf expressions-subl (rest expressions-subl)))))))))

(defmethod compile-parenscript-form ((form cons) &key (expecting :statement))
  (let* ((name (car form))
	 (args (cdr form)))
    (cond ((eql name 'quote)
           (assert (= 1 (length args)) () "Wrong number of arguments to quote: ~s" args)
           (list 'script-quote (first args)))
          ((ps-special-form-p form) (apply (get-ps-special-form name) (cons expecting args)))
          ((op-form-p form)
           (list 'operator
                 (script-convert-op-name (compile-parenscript-form (first form) :expecting :symbol))
                 (mapcar (lambda (form) (compile-parenscript-form form :expecting :expression)) (rest form))))
          ((method-call-p form)
           (list 'js-method-call
                 (compile-parenscript-form name :expecting :symbol)
                 (compile-parenscript-form (first args) :expecting :expression)
                 (compile-function-argument-forms (rest args))))
          ((funcall-form-p form)
           (list 'js-funcall
                 (compile-parenscript-form name :expecting :expression)
                 (compile-function-argument-forms args)))
          (t (error "Cannot compile ~S to a ParenScript form." form)))))

