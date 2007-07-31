(in-package :parenscript)

;;;; The macrology of the Parenscript language.  Special forms and macros.

;;; parenscript gensyms
(defvar *gen-script-name-counter* 0)

(defun gen-script-name-string (&key (prefix "_js_"))
  "Generates a unique valid javascript identifier ()"
  (concatenate 'string
               prefix (princ-to-string (incf *gen-script-name-counter*))))

(defun gen-script-name (&key (prefix "_ps_"))
  "Generate a new javascript identifier."
  (intern (gen-script-name-string :prefix prefix)
          (find-package :js)))

(defmacro gen-ps-name (&rest args)
  `(gen-script-name ,@args))

(defmacro with-unique-ps-names (symbols &body body)
  "Evaluate BODY with the variables on SYMBOLS bound to new javascript identifiers.

Each element of SYMBOLS is either a symbol or a list of (symbol
prefix)."
  `(let* ,(mapcar (lambda (symbol)
                    (destructuring-bind (symbol &optional prefix)
                        (if (consp symbol)
                            symbol
                            (list symbol))
                      (if prefix
                          `(,symbol (gen-script-name :prefix ,prefix))
                          `(,symbol (gen-script-name)))))
                  symbols)
     ,@body))

(defvar *var-counter* 0)

(defun script-gensym (&optional (name "js"))
  (intern (format nil "tmp-~A-~A" name (incf *var-counter*)) #.*package*))

;;; array literals
(defscriptmacro list (&rest values)
  `(array ,@values))

(defscriptmacro make-array (&rest inits)
  `(new (*array ,@inits)))

;;; eval-when
(define-script-special-form eval-when (&rest args)
  "(eval-when form-language? (situation*) form*)

The given forms are evaluated only during the given SITUATION in the specified 
FORM-LANGUAGE (either :lisp or :parenscript, def, defaulting to :lisp during
-toplevel and :parenscript during :execute). The accepted SITUATIONS are :execute,
:scan-toplevel. :scan-toplevel is the phase of compilation when function definitions 
and the like are being added to the compilation environment. :execute is the phase when
the code is being evaluated by a Javascript engine."
  (multiple-value-bind (body-language situations subforms)
      (process-eval-when-args args)
;    (format t "~A~%~A~%"
;	   (and (compiler-in-situation-p *compilation-environment* :compile-toplevel)
;		(find :compile-toplevel situations))
;	   (compiler-in-situation-p *compilation-environment*  :execute)
;	    (find :execute situations))
    (cond
      ((and (compiler-in-situation-p *compilation-environment* :compile-toplevel)
	    (find :compile-toplevel situations))
       (error "Should never be processing eval-when :COMPILE-TOPLEVEL forms from here."))

      ((and (compiler-in-situation-p *compilation-environment*  :execute)
	    (find :execute situations))
       (when (eql body-language :parenscript)
	 (let ((form `(progn ,@subforms)))
;	   (format t "Form: ~A~%" form)
	   (compile-to-statement form)))))))

;;; script packages
(defscriptmacro defpackage (name &rest options)
  "Defines a Parenscript package."
  (labels ((opt-name (opt) (if (listp opt) (car opt) opt)))
  (let ((nicknames nil) (lisp-package nil) (secondary-lisp-packages nil)
	(exports nil) (used-packages nil) (documentation nil))
    (dolist (opt options)
      (case (opt-name opt)
	(:lisp-package (setf lisp-package (second opt)))
	(:nicknames (setf nicknames (rest opt)))
	(:secondary-lisp-packages secondary-lisp-packages t)
	(:export (setf exports (rest opt)))
	(:use (setf used-packages (rest opt)))
	(:documentation (setf documentation (second opt)))
	(t (error "Unknown option in DEFPACKAGE: ~A" (opt-name opt)))))
;    (format t "Exports: ~A~%" exports)
    (create-script-package
     *compilation-environment*
     :name name
     :nicknames nicknames
     :secondary-lisp-packages secondary-lisp-packages
     :used-packages used-packages
     :lisp-package lisp-package
     :exports exports
     :documentation documentation)))
  `(progn))

(defscriptmacro in-package (package-designator)
  "Changes the current script package in the parenscript compilation environment.  This mostly
affects the reader and how it interns non-prefixed symbols"
  (let ((script-package
	 (find-script-package package-designator *compilation-environment*)))
    (when (null script-package)
      (error "~A does not designate any script package.  Available script package: ~A"
	     package-designator
	     (mapcar #'script-package-name (comp-env-script-packages *compilation-environment*))))
    (setf (comp-env-current-package *compilation-environment*)
	  script-package)
    `(progn)))

(defscriptmacro case (value &rest clauses)
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

;;; let
(define-script-special-form let (decls &rest body)
  (let ((defvars (mapcar #'(lambda (decl)
			     (if (atom decl)
                                 (make-instance 'ps-js::js-defvar
                                       :names (list (compile-to-symbol decl))
                                       :value nil)
                                 (let ((name (first decl))
                                       (value (second decl)))
                                   (make-instance 'ps-js::js-defvar
                                                  :names (list (compile-to-symbol name))
                                                  :value (compile-to-expression value)))))
			 decls)))
    (make-instance 'ps-js::js-sub-block
		   :indent "  "
		   :statements (nconc defvars
				 (mapcar #'compile-to-statement body)))))

;;; iteration
(defscriptmacro dotimes (iter &rest body)
  (let ((var (first iter))
        (times (second iter)))
  `(do ((,var 0 (1+ ,var)))
       ((>= ,var ,times))
     ,@body)))

(defscriptmacro dolist (i-array &rest body)
  (let ((var (first i-array))
	(array (second i-array))
	(arrvar (script-gensym "arr"))
	(idx (script-gensym "i")))
    `(let ((,arrvar ,array))
      (do ((,idx 0 (1+ ,idx)))
	  ((>= ,idx (slot-value ,arrvar 'length)))
	(let ((,var (aref ,arrvar ,idx)))
	  ,@body)))))

;;; macros
(defmacro with-temp-macro-environment ((var) &body body)
  `(let* ((,var (make-macro-env-dictionary))
          (*script-macro-env* (cons ,var *script-macro-env*)))
    ,@body))

(define-script-special-form macrolet (macros &body body)
  (with-temp-macro-environment (macro-env-dict)
    (dolist (macro macros)
      (destructuring-bind (name arglist &body body)
          macro
	(setf (get-macro-spec name macro-env-dict)
	      (cons nil (let ((args (gensym "ps-macrolet-args-")))
                          (compile nil `(lambda (&rest ,args)
                                         (destructuring-bind ,arglist
                                             ,args
                                           ,@body))))))))
    (compile-script-form `(progn ,@body))))

(define-script-special-form symbol-macrolet (symbol-macros &body body)
  (with-temp-macro-environment (macro-env-dict)
    (dolist (macro symbol-macros)
      (destructuring-bind (name &body expansion)
          macro
	(setf (get-macro-spec name macro-env-dict)
	      (cons t (compile nil `(lambda () ,@expansion))))))
    (compile-script-form `(progn ,@body))))

(defscriptmacro defmacro (name args &body body)
  `(lisp (defscriptmacro ,name ,args ,@body) nil))

(defscriptmacro lisp (&body forms)
  "Evaluates the given forms in Common Lisp at ParenScript
macro-expansion time. The value of the last form is treated as a
ParenScript expression and is inserted into the generated Javascript
\(use nil for no-op)."
  (eval (cons 'progn forms)))

(defscriptmacro rebind (variables &body body)
  "Creates a new js lexical environment and copies the given
variable(s) there. Executes the body in the new environment. This
has the same effect as a new (let () ...) form in lisp but works on
the js side for js closures."
  (unless (listp variables)
    (setf variables (list variables)))
  `((lambda ()
      (let ((new-context (new *object)))
        ,@(loop for variable in variables
                collect `(setf (slot-value new-context ,(symbol-to-js variable))
                               ,variable))
        (with new-context
          ,@body)))))
