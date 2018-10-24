;;; Copyright 2005 Manuel Odendahl
;;; Copyright 2005-2006 Edward Marco Baringer
;;; Copyright 2006 Attila Lendvai
;;; Copyright 2006 Luca Capello
;;; Copyright 2007-2012 Vladimir Sedach
;;; Copyright 2008 Travis Cross
;;; Copyright 2009-2010 Red Daly
;;; Copyright 2009-2010 Daniel Gackle

;;; SPDX-License-Identifier: BSD-3-Clause

;;; Redistribution and use in source and binary forms, with or
;;; without modification, are permitted provided that the following
;;; conditions are met:

;;; 1. Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.

;;; 2. Redistributions in binary form must reproduce the above
;;; copyright notice, this list of conditions and the following
;;; disclaimer in the documentation and/or other materials provided
;;; with the distribution.

;;; 3. Neither the name of the copyright holder nor the names of its
;;; contributors may be used to endorse or promote products derived
;;; from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package #:parenscript)
(in-readtable :parenscript)

(defvar *version* 2.7 "Parenscript compiler version.")

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
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (gethash ',name *macro-toplevel*) ,macro-fn-form)
           (setf (gethash ',name *macro-toplevel-lambda-list*) ',effective-lambda-list)
           ',name))))

(defmacro define-ps-symbol-macro (symbol expansion)
  (defined-operator-override-check symbol
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (gethash ',symbol *symbol-macro-toplevel*) (lambda (form) (declare (ignore form)) ',expansion)))))

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

;;; lambda wrapping

(defvar *ps-gensym-counter* 0)

(defvar this-in-lambda-wrapped-form? nil)

(defun lambda-wrap (form)
  (let ((this-in-lambda-wrapped-form? :query)
	(*ps-gensym-counter* *ps-gensym-counter*))
    (ps-compile form)
    (if (eq this-in-lambda-wrapped-form? :yes)
	`(chain (lambda () ,form) (call this))
	`((lambda () ,form)))))

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
    (cond ((or (not compile-expression?) this-in-lambda-wrapped-form?)
           (apply (or statement-impl expression-impl) (cdr form)))
          (expression-impl
           (apply expression-impl (cdr form)))
          ((member op *lambda-wrappable-statements*)
           (compile-expression (lambda-wrap form)))
          (t (error 'compile-expression-error :form form)))))

(defun ps-compile (form)
  (macrolet ((try-expanding (form &body body)
               `(multiple-value-bind (expansion expanded?) (ps-macroexpand ,form)
                  (if expanded?
                      (ps-compile expansion)
                      (progn ,@body)))))
    (typecase form
      ((or null number string character)
       form)
      (vector
       (ps-compile `(quote ,(coerce form 'list))))
      (symbol
       (try-expanding form
	 (when (and (eq form 'this) (eq this-in-lambda-wrapped-form? :query))
	   (setq this-in-lambda-wrapped-form? :yes))
	 form))
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

(defun ps-gensym (&optional (x '_js))
  (make-symbol
   (if (integerp x)
       (format nil "~A~A" '_js x)
       (let ((prefix (string x)))
         (format nil "~A~:[~;_~]~A"
                 prefix
                 (digit-char-p (char prefix (1- (length prefix))))
                 (incf *ps-gensym-counter*))))))

(defmacro with-ps-gensyms (symbols &body body)
  "Helper macro for writing Parenscript macros. Each element of
SYMBOLS is either a symbol or a list of (symbol
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
  "Helper macro for writing Parenscript macros. Useful for preventing unwanted multiple evaluation."
  (warn-deprecated 'ps-once-only 'maybe-once-only)
  (let ((gensyms (mapcar #'ps-gensym vars)))
    `(let* ,(mapcar (lambda (g v) `(,g (ps-gensym ',v)))
                    gensyms vars)
       `(let* (,,@(mapcar (lambda (g v) `(list ,g ,v))
                          gensyms vars))
          ,(let* ,(mapcar (lambda (g v) (list v g))
                          gensyms vars)
             ,@body)))))

(defmacro maybe-once-only ((&rest vars) &body body)
  "Helper macro for writing Parenscript macros. Like PS-ONCE-ONLY,
except that if the given VARS are variables or constants, no intermediate variables are created."
  (let ((vars-bound (gensym)))
    `(let*
         ((,vars-bound ())
          ,@(loop for var in vars collect
                 `(,var
                   (if (or
                        (constantp ,var)
                        (and
                         (symbolp ,var)
                         (not (lookup-macro-def ,var *symbol-macro-env*))
                         (not (gethash ,var *symbol-macro-toplevel*))))
                        ,var
                        (let ((var¹ (ps-gensym ',var)))
                          (push (list var¹ ,var) ,vars-bound)
                          var¹)))))
       `(let ,(reverse ,vars-bound)
          ,,@body))))
