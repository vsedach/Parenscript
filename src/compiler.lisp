(in-package #:parenscript)
(in-readtable :parenscript)

(defvar *version* 2.3 "Parenscript compiler version.")

(defparameter %compiling-reserved-forms-p% t
  "Used to issue warnings when replacing PS special operators or macros.")

(defvar *defined-operators* ()
  "Special operators and macros defined by Parenscript. Replace at your own risk!")

(defun defined-operator-override-check (name &rest body)
  (when (and (not %compiling-reserved-forms-p%) (member name *defined-operators*))
    (warn 'simple-style-warning
          :format-control "Redefining Parenscript operator/macro ~A"
          :format-arguments (list name)))
  `(progn ,(when %compiling-reserved-forms-p% `(pushnew ',name *defined-operators*))
          ,@body))

(defvar *reserved-symbol-names*
  (list "break" "case" "catch" "continue" "default" "delete" "do" "else"
        "finally" "for" "function" "if" "in" "instanceof" "new" "return"
        "switch" "this" "throw" "try" "typeof" "var" "void" "while" "with"
        "abstract" "boolean" "byte" "char" "class" "const" "debugger" "double"
        "enum" "export" "extends" "final" "float" "goto" "implements" "import"
        "int" "interface" "long" "native" "package" "private" "protected"
        "public" "short" "static" "super" "synchronized" "throws" "transient"
        "volatile" "{}" "true" "false" "null" "undefined"))

(defvar *lambda-wrappable-statements* ;; break, return, continue not included
  '(throw switch for for-in while try block))

(defun reserved-symbol-p (symbol)
  (find (string-downcase (string symbol)) *reserved-symbol-names* :test #'string=))

;;; special forms

(defvar *special-expression-operators* (make-hash-table :test 'eq))
(defvar *special-statement-operators* (make-hash-table :test 'eq))

;; need to split special op definition into two parts - statement and expression
(defmacro %define-special-operator (type name lambda-list &body body)
  (defined-operator-override-check name
      `(setf (gethash ',name ,type)
             (lambda (&rest whole)
               (destructuring-bind ,lambda-list whole
                 ,@body)))))

(defmacro define-expression-operator (name lambda-list &body body)
  `(%define-special-operator *special-expression-operators*
       ,name ,lambda-list ,@body))

(defmacro define-statement-operator (name lambda-list &body body)
  `(%define-special-operator *special-statement-operators*
       ,name ,lambda-list ,@body))

(defun special-form? (form)
  (and (consp form)
       (symbolp (car form))
       (or (gethash (car form) *special-expression-operators*)
           (gethash (car form) *special-statement-operators*))))

;;; scoping and lexical environment

(defvar *vars-needing-to-be-declared* ()
  "This special variable is expected to be bound to a fresh list by
special forms that introduce a new JavaScript lexical block (currently
function definitions and lambdas). Enclosed special forms are expected
to push variable declarations onto the list when the variables
declaration cannot be made by the enclosed form (for example, a x,y,z
expression progn). It is then the responsibility of the enclosing
special form to introduce the variable declarations in its lexical
block.")

(defvar *used-up-names*)
(setf (documentation '*used-up-names* 'variable)
      "Names that have been already used for lexical bindings in the current function scope.")

(defvar in-case? nil
  "Bind to T when compiling CASE branches.")

(defvar in-loop-scope? nil
  "Used for seeing when we're in loops, so that we can introduce
  proper scoping for lambdas closing over loop-bound
  variables (otherwise they all share the same binding).")
(defvar *loop-return-var* nil
  "Variable which is used to return values from inside loop bodies.")
(defvar *loop-return-set-var* nil
  "Variable which is set by RETURN-FROM when it returns a value from inside
  a loop.  The value is the name of a PS variable which dynamically
  indicates if the return statement indeed has been invoked.")

(defvar *loop-scope-lexicals*)
(setf (documentation '*loop-scope-lexicals* 'variable)
      "Lexical variables introduced by a loop.")
(defvar *loop-scope-lexicals-captured*)
(setf (documentation '*loop-scope-lexicals-captured* 'variable)
      "Lexical variables introduced by a loop that are also captured by lambdas inside a loop.")

(defvar in-function-scope? nil
  "Lets the compiler know when lambda wrapping is necessary.")

(defvar *local-function-names* ()
  "Functions named by flet and label.")
;; is a subset of
(defvar *enclosing-lexicals* ()
  "All enclosing lexical variables (includes function names).")
(defvar *enclosing-function-arguments* ()
  "Lexical variables bound in all lexically enclosing function argument lists.")

(defvar *function-block-names* ()
  "All block names that this function is responsible for catching.")
(defvar *dynamic-return-tags* ()
  "Tags that need to be thrown to to reach.")
(defvar *current-block-tag* nil
  "Name of the lexically enclosing block, if any.")

(defvar *special-variables* ()
  "Special variables declared during any Parenscript run. Re-bind this if you want to clear the list.")

(defun special-variable? (sym)
  (member sym *special-variables*))

;;; meta info

(defvar *macro-toplevel-lambda-list* (make-hash-table)
  "Table of lambda lists for toplevel macros.")

(defvar *function-lambda-list* (make-hash-table)
  "Table of lambda lists for defined functions.")

;;; macros
(defun make-macro-dictionary ()
  (make-hash-table :test 'eq))

(defvar *macro-toplevel* (make-macro-dictionary)
  "Toplevel macro environment dictionary.")

(defvar *macro-env* (list *macro-toplevel*)
  "Current macro environment.")

(defvar *symbol-macro-toplevel* (make-macro-dictionary))

(defvar *symbol-macro-env* (list *symbol-macro-toplevel*))

(defvar *setf-expanders* (make-macro-dictionary)
  "Setf expander dictionary. Key is the symbol of the access
function of the place, value is an expansion function that takes the
arguments of the access functions as a first value and the form to be
stored as the second value.")

(defun lookup-macro-def (name env)
  (loop for e in env thereis (gethash name e)))

(defun make-ps-macro-function (args body)
  "Given the arguments and body to a parenscript macro, returns a
function that may be called on the entire parenscript form and outputs
some parenscript code.  Returns a second value that is the effective
lambda list from a Parenscript perspective."
  (let* ((whole-var (when (eql '&whole (first args)) (second args)))
         (effective-lambda-list (if whole-var (cddr args) args))
         (whole-arg (or whole-var (gensym "ps-macro-form-arg-"))))
    (values
     `(lambda (,whole-arg)
        (destructuring-bind ,effective-lambda-list
            (cdr ,whole-arg)
          ,@body))
     effective-lambda-list)))

(defmacro defpsmacro (name args &body body)
  (defined-operator-override-check name
      (multiple-value-bind (macro-fn-form effective-lambda-list)
          (make-ps-macro-function args body)
        `(progn
           (setf (gethash ',name *macro-toplevel*) ,macro-fn-form)
           (setf (gethash ',name *macro-toplevel-lambda-list*) ',effective-lambda-list)
           ',name))))

(defmacro define-ps-symbol-macro (symbol expansion)
  (defined-operator-override-check symbol
      `(setf (gethash ',symbol *symbol-macro-toplevel*) (lambda (form) (declare (ignore form)) ',expansion))))

(defun import-macros-from-lisp (&rest names)
  "Import the named Lisp macros into the Parenscript macro
environment. When the imported macro is macroexpanded by Parenscript,
it is first fully macroexpanded in the Lisp macro environment, and
then that expansion is further expanded by Parenscript."
  (dolist (name names)
    (eval `(defpsmacro ,name (&rest args)
             (macroexpand `(,',name ,@args))))))

(defmacro defmacro+ps (name args &body body)
  "Define a Lisp macro and a Parenscript macro with the same macro
function (ie - the same result from macroexpand-1), for cases when the
two have different full macroexpansions (for example if the CL macro
contains implementation-specific code when macroexpanded fully in the
CL environment)."
  `(progn (defmacro ,name ,args ,@body)
          (defpsmacro ,name ,args ,@body)))

(defun ps-macroexpand-1 (form)
  (aif (or (and (symbolp form)
                (or (and (member form *enclosing-lexicals*)
                         (lookup-macro-def form *symbol-macro-env*))
                    (gethash form *symbol-macro-toplevel*))) ;; hack
           (and (consp form) (lookup-macro-def (car form) *macro-env*)))
       (values (ps-macroexpand (funcall it form)) t)
       form))

(defun ps-macroexpand (form)
  (multiple-value-bind (form1 expanded?) (ps-macroexpand-1 form)
    (if expanded?
        (values (ps-macroexpand form1) t)
        form1)))

;;;; compiler interface

(defparameter *compilation-level* :toplevel
  "This value takes on the following values:
:toplevel indicates that we are traversing toplevel forms.
:inside-toplevel-form indicates that we are inside a call to ps-compile-*
nil indicates we are no longer toplevel-related.")

(defun adjust-compilation-level (form level)
  "Given the current *compilation-level*, LEVEL, and the fully macroexpanded
form, FORM, returns the new value for *compilation-level*."
  (cond ((or (and (consp form)
                  (member (car form) '(progn locally macrolet symbol-macrolet)))
             (and (symbolp form) (eq :toplevel level)))
         level)
        ((eq :toplevel level) :inside-toplevel-form)))

(defvar compile-expression?)

(define-condition compile-expression-error (error)
  ((form :initarg :form :reader error-form))
  (:report (lambda (condition stream)
             (format stream "The Parenscript form ~A cannot be compiled into an expression." (error-form condition)))))

(defun compile-special-form (form)
  (let* ((op (car form))
         (statement-impl (gethash op *special-statement-operators*))
         (expression-impl (gethash op *special-expression-operators*)))
    (cond ((not compile-expression?)
           (apply (or statement-impl expression-impl) (cdr form)))
          (expression-impl
           (apply expression-impl (cdr form)))
          ((member op *lambda-wrappable-statements*)
           (compile-expression `((lambda () ,form))))
          (t (error 'compile-expression-error :form form)))))

(defun ps-compile (form)
  (macrolet ((try-expanding (form &body body)
               `(multiple-value-bind (expansion expanded?) (ps-macroexpand ,form)
                  (if expanded?
                      (ps-compile expansion)
                      ,@body))))
    (typecase form
      ((or null number string character)
       form)
      (vector
       (ps-compile `(quote ,(coerce form 'list))))
      (symbol
       (try-expanding form form))
      (cons
       (try-expanding form
         (let ((*compilation-level*
                (adjust-compilation-level form *compilation-level*)))
           (if (special-form? form)
               (compile-special-form form)
               `(ps-js:funcall
                 ,(if (symbolp (car form))
                      (maybe-rename-local-function (car form))
                      (compile-expression (car form)))
                 ,@(mapcar #'compile-expression (cdr form))))))))))

(defun compile-statement (form)
  (let ((compile-expression? nil))
    (ps-compile form)))

(defun compile-expression (form)
  (let ((compile-expression? t))
    (ps-compile form)))

(defvar *ps-gensym-counter* 0)

(defun ps-gensym (&optional (prefix-or-counter "_JS"))
  (assert (or (stringp prefix-or-counter) (integerp prefix-or-counter)))
  (let ((prefix (if (stringp prefix-or-counter) prefix-or-counter "_JS"))
        (counter (if (integerp prefix-or-counter) prefix-or-counter (incf *ps-gensym-counter*))))
   (make-symbol (format nil "~A~:[~;_~]~A" prefix
                        (digit-char-p (char prefix (1- (length prefix))))
                        counter))))

(defmacro with-ps-gensyms (symbols &body body)
  "Each element of SYMBOLS is either a symbol or a list of (symbol
gensym-prefix-string)."
  `(let* ,(mapcar (lambda (symbol)
                    (destructuring-bind (symbol &optional prefix)
                        (if (consp symbol)
                            symbol
                            (list symbol))
                      (if prefix
                          `(,symbol (ps-gensym ,(string prefix)))
                          `(,symbol (ps-gensym ,(string symbol))))))
                  symbols)
     ,@body))

(defmacro ps-once-only ((&rest vars) &body body)
  (let ((gensyms (mapcar (lambda (x) (ps-gensym (string x))) vars)))
    `(let ,(mapcar (lambda (g v) `(,g (ps-gensym ,(string v)))) gensyms vars)
       `(let* (,,@(mapcar (lambda (g v) ``(,,g ,,v)) gensyms vars))
          ,(let ,(mapcar (lambda (g v) `(,v ,g)) gensyms vars)
             ,@body)))))
