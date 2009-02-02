(in-package :parenscript)

;;; reserved symbols/literals

(defvar *ps-reserved-symbol-names* ()) ;; symbol names reserved for PS/JS literals

(defun add-ps-literal (name)
  (push (symbol-name name) *ps-reserved-symbol-names*))

(defun ps-literal-p (symbol)
  (find (symbol-name symbol) *ps-reserved-symbol-names* :test #'equalp))

;;; special forms

(defvar *ps-special-forms* (make-hash-table :test 'eq))

(defun get-ps-special-form (name)
  (gethash name *ps-special-forms*))

(defmacro define-ps-special-form (name lambda-list &rest body)
  "Define a special form NAME. The first argument given to the special
form is a keyword indicating whether the form is expected to produce
an :expression or a :statement. The resulting Parenscript language
types are appended to the ongoing javascript compilation."
  (let ((arglist (gensym "ps-arglist-")))
    `(setf (gethash ',name *ps-special-forms*)
           (lambda (&rest ,arglist)
             (destructuring-bind ,lambda-list
                 ,arglist
               ,@body)))))

(defun undefine-ps-special-form (name)
  (remhash name *ps-special-forms*))

(defun ps-special-form-p (form)
  (and (consp form)
       (symbolp (car form))
       (gethash (car form) *ps-special-forms*)))

;;; scoping

(defvar *enclosing-lexical-block-declarations* ()
  "This special variable is expected to be bound to a fresh list by
special forms that introduce a new JavaScript lexical block (currently
function definitions and lambdas). Enclosed special forms are expected
to push variable declarations onto the list when the variables
declaration cannot be made by the enclosed form \(for example, a
\(x,y,z\) expression progn\). It is then the responsibility of the
enclosing special form to introduce the variable bindings in its
lexical block.")

(defvar *ps-special-variables* ())

;;; form predicates

(defun op-form-p (form)
  (and (listp form)
       (not (ps-special-form-p form))
       (not (null (op-precedence (first form))))))

(defun funcall-form-p (form)
  (and (listp form)
       (not (op-form-p form))
       (not (ps-special-form-p form))))

;;; macro expansion
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-macro-env-dictionary ()
    (make-hash-table :test 'eq))
  (defvar *ps-macro-toplevel* (make-macro-env-dictionary)
    "Toplevel macro environment dictionary. Key is the symbol name of
    the macro, value is (symbol-macro-p . expansion-function).")
  (defvar *ps-macro-env* (list *ps-macro-toplevel*)
    "Current macro environment.")

  (defvar *ps-setf-expanders* (make-macro-env-dictionary)
    "Setf expander dictionary. Key is the symbol of the access
function of the place, value is an expansion function that takes the
arguments of the access functions as a first value and the form to be
stored as the second value.")
  
  (defun get-macro-spec (name env-dict)
    "Retrieves the macro spec of the given name with the given environment dictionary.
SPEC is of the form (symbol-macro-p . expansion-function)."
    (gethash name env-dict))
  (defsetf get-macro-spec (name env-dict)
      (spec)
    `(setf (gethash ,name ,env-dict) ,spec)))

(defun lookup-macro-spec (name &optional (environment *ps-macro-env*))
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
                            (list *ps-macro-toplevel*)))))))))

(defun ps-symbol-macro-p (name &optional (environment *ps-macro-env*))
  "True if there is a Parenscript symbol macro named by the symbol NAME."
  (and (symbolp name) (car (lookup-macro-spec name environment))))

(defun ps-macro-p (name &optional (environment *ps-macro-env*))
  "True if there is a Parenscript macro named by the symbol NAME."
  (and (symbolp name)
       (let ((macro-spec (lookup-macro-spec name environment)))
         (and macro-spec (not (car macro-spec))))))

(defun lookup-macro-expansion-function (name &optional (environment *ps-macro-env*))
  "Lookup NAME in the given macro expansion environment (which
defaults to the current macro environment). Returns the expansion
function and the parent macro environment of the macro."
  (multiple-value-bind (macro-spec parent-env)
      (lookup-macro-spec name environment)
    (values (cdr macro-spec) parent-env)))

(defun make-ps-macro-function (args body)
  (let* ((whole-var (when (eql '&whole (first args)) (second args)))
         (effective-lambda-list (if whole-var (cddr args) args))
         (whole-arg (or whole-var (gensym "ps-macro-form-arg-"))))
    `(lambda (,whole-arg)
       (destructuring-bind ,effective-lambda-list
           (cdr ,whole-arg)
         ,@body))))

(defmacro defpsmacro (name args &body body)
  `(progn (undefine-ps-special-form ',name)
          (setf (get-macro-spec ',name *ps-macro-toplevel*)
                (cons nil ,(make-ps-macro-function args body)))
          ',name))

(defmacro define-ps-symbol-macro (symbol expansion)
  `(progn (undefine-ps-special-form ',symbol)
          (setf (get-macro-spec ',symbol *ps-macro-toplevel*) (cons t (lambda () ',expansion)))
          ',symbol))

(defun import-macros-from-lisp (&rest names)
  "Import the named Lisp macros into the ParenScript macro
environment. When the imported macro is macroexpanded by ParenScript,
it is first fully macroexpanded in the Lisp macro environment, and
then that expansion is further expanded by ParenScript."
  (dolist (name names)
    (eval `(defpsmacro ,name (&rest args)
             (macroexpand `(,',name ,@args))))))

(defmacro defmacro/ps (name args &body body)
  "Define a Lisp macro and import it into the ParenScript macro environment."
  `(progn (defmacro ,name ,args ,@body)
          (import-macros-from-lisp ',name)))

(defmacro defmacro+ps (name args &body body)
  "Define a Lisp macro and a ParenScript macro with the same macro
function (ie - the same result from macroexpand-1), for cases when the
two have different full macroexpansions (for example if the CL macro
contains implementation-specific code when macroexpanded fully in the
CL environment)."
  `(progn (defmacro ,name ,args ,@body)
          (defpsmacro ,name ,args ,@body)))

(defun ps-macroexpand (form)
  "Recursively macroexpands ParenScript macros and symbol-macros in
the given ParenScript form. Returns two values: the expanded form, and
whether any expansion was performed on the form or not."
  (if (consp form)
      (let ((op (car form))
            (args (cdr form)))
        (cond ((equal op 'quote) (values (if (equalp '(nil) args) nil form) ; leave quotes alone, unless it's a quoted nil
                                         nil))
              ((ps-macro-p op) (values (ps-macroexpand (funcall (lookup-macro-expansion-function op) form)) t))
              (t (values form nil))))
      (cond ((ps-symbol-macro-p form) (values (ps-macroexpand (funcall (lookup-macro-expansion-function form))) t))
            (t (values form nil)))))

;;;; compiler interface
(defgeneric compile-parenscript-form (form &key expecting)
  (:documentation "Compiles a ParenScript form to the intermediate
ParenScript representation. :expecting determines whether the form is
compiled to an :expression (the default), a :statement, or a
:symbol."))

(defmethod compile-parenscript-form :around (form &key expecting)
  (assert (if expecting (member expecting '(:expression :statement :symbol)) t))
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
              (eql (first exp) 'ps-quote))
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
  (cond ((keywordp symbol) symbol)
        ((ps-special-form-p (list symbol))
         (if (ps-literal-p symbol)
             (funcall (get-ps-special-form symbol) :symbol)
             (error "Attempting to use Parenscript special form ~a as variable" symbol)))
        (t (list 'js-variable symbol))))

(defun compile-function-argument-forms (args)
  (let ((remaining-args args))
    (loop while remaining-args collecting
         (if (keywordp (first remaining-args))
             (prog2 (when (oddp (length remaining-args))
                      (error "Odd number of keyword arguments: ~A." args))
                 (compile-parenscript-form (cons 'create remaining-args) :expecting :expression)
               (setf remaining-args nil))
             (prog1 (compile-parenscript-form (first remaining-args) :expecting :expression)
               (setf remaining-args (cdr remaining-args)))))))

(defun ps-convert-op-name (op)
  (case (ensure-ps-symbol op)
    (and '\&\&)
    (or '\|\|)
    (not '!)
    (eql '\=\=)
    (=   '\=\=)
    (t op)))

(defmethod compile-parenscript-form ((form cons) &key (expecting :statement))
  (let* ((name (car form))
         (args (cdr form)))
    (cond ((eql name 'quote)
           (assert (= 1 (length args)) () "Wrong number of arguments to quote: ~s" args)
           (list 'ps-quote (first args)))
          ((ps-special-form-p form) (apply (get-ps-special-form name) (cons expecting args)))
          ((op-form-p form)
           (list 'operator
                 (ps-convert-op-name (compile-parenscript-form (first form) :expecting :symbol))
                 (mapcar (lambda (form) (compile-parenscript-form form :expecting :expression)) (rest form))))
          ((funcall-form-p form)
           (list 'js-funcall
                 (compile-parenscript-form name :expecting :expression)
                 (compile-function-argument-forms args)))
          (t (error "Cannot compile ~S to a ParenScript form." form)))))

(defvar *ps-gensym-counter* 0)

(defun ps-gensym (&optional (prefix "_js"))
  (make-symbol (format nil "~A~A" prefix (incf *ps-gensym-counter*))))

(defmacro with-ps-gensyms (symbols &body body)
  "Evaluate BODY with SYMBOLS bound to unique ParenScript identifiers.

Each element of SYMBOLS is either a symbol or a list of (symbol
gensym-prefix-string)."
  `(let* ,(mapcar (lambda (symbol)
                    (destructuring-bind (symbol &optional prefix)
                        (if (consp symbol)
                            symbol
                            (list symbol))
                      (if prefix
                          `(,symbol (ps-gensym ,prefix))
                          `(,symbol (ps-gensym ,(symbol-to-js-string symbol))))))
                  symbols)
     ,@body))

(defun %check-once-only-vars (vars)
  (let ((bad-var (find-if (lambda (x) (or (not (symbolp x)) (keywordp x))) vars)))
    (when bad-var
      (error "PS-ONLY-ONCE expected a non-keyword symbol but got ~s" bad-var))))

(defmacro ps-once-only ((&rest vars) &body body)
  (%check-once-only-vars vars)
  (let ((gensyms (mapcar (lambda (x) (ps-gensym (string x))) vars)))
    `(let ,(mapcar (lambda (g v) `(,g (ps-gensym ,(string v)))) gensyms vars)
       `(let* (,,@(mapcar (lambda (g v) ``(,,g ,,v)) gensyms vars))
          ,(let ,(mapcar (lambda (g v) `(,v ,g)) gensyms vars)
             ,@body)))))
