(in-package :parenscript)

;;;; The macrology of the Parenscript language.  Special forms and macros.

;;; parenscript gensyms
(defvar *gen-script-name-counter* 0)

(defun gen-script-name-string (&key (prefix "_js_"))
  "Generates a unique valid javascript identifier ()"
  (concatenate 'string
               prefix (princ-to-string (incf *gen-script-name-counter*))))

(defun gen-script-name (&key (prefix ""))
  "Generate a new javascript identifier."
  (intern (gen-script-name-string :prefix prefix)
          (find-package :parenscript.ps-gensyms)))

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

(defscriptmacro defaultf (place value)
  `(setf ,place (or (and (=== undefined ,place) ,value)
		 ,place)))

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
    (cond
      ((and (compiler-in-situation-p *compilation-environment* :compile-toplevel)
	    (find :compile-toplevel situations))
       (error "Should never be processing eval-when :COMPILE-TOPLEVEL forms from here."))

      ((and (compiler-in-situation-p *compilation-environment*  :execute)
	    (find :execute situations))
       (when (eql body-language :parenscript)
	 (let ((form `(progn ,@subforms)))
	   (compile-to-statement form)))))))

;;; slot access
(defscriptmacro slot-value (obj &rest slots)
  (if (null (rest slots))
      `(%js-slot-value ,obj ,(first slots))
      `(slot-value (slot-value ,obj ,(first slots)) ,@(rest slots))))

(defscriptmacro with-slots (slots object &rest body)
  (flet ((slot-var (slot) (if (listp slot) (first slot) slot))
	 (slot-symbol (slot) (if (listp slot) (second slot) slot)))
    `(symbol-macrolet ,(mapcar #'(lambda (slot)
				   `(,(slot-var slot) '(slot-value ,object ',(slot-symbol slot))))
			       slots)
      ,@body)))

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
	  ((>= ,idx (slot-value ,arrvar 'global::length)))
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

(defscriptmacro define-symbol-macro (name &body body)
  `(lisp (define-script-symbol-macro ,name ,@body)))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-function-body (body)
    ;; (format t "parsing function body ~A~%" body)
    (let* ((documentation
	    (when (stringp (first body))
	      (first body)))
	   (body-forms (if documentation (rest body) body)))
      (values
       body-forms
       documentation)))

  (defun parse-key-spec (key-spec)
    "parses an &key parameter.  Returns 4 values:
var, init-form,  keyword-name, supplied-p-var, init-form-supplied-p.

Syntax of key spec:
[&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}*
"
    (let* ((var (cond ((symbolp key-spec) key-spec)
		      ((and (listp key-spec) (symbolp (first key-spec))) (first key-spec))
		      ((and (listp key-spec) (listp (first key-spec)))   (second key-spec))))
	   (keyword-name (if (and (listp key-spec) (listp (first key-spec)))
			     (first (first key-spec))
			     (intern (string var) :keyword)))
	   (init-form (if (listp key-spec) (second key-spec) nil))
	   (init-form-supplied-p (if (listp key-spec) t nil))
	   (supplied-p-var (if (listp key-spec) (third key-spec) nil)))
      (values var init-form keyword-name supplied-p-var init-form-supplied-p)))

  (defun parse-optional-spec (spec)
    "Parses an &optional parameter.  Returns 3 values: var, init-form, supplied-p-var.
[&optional {var | (var [init-form [supplied-p-parameter]])}*] "
    (let* ((var (cond ((symbolp spec) spec)
		      ((and (listp spec) (first spec)))))
	   (init-form (if (listp spec) (second spec)))
	   (supplied-p-var (if (listp spec) (third spec))))
      (values var init-form supplied-p-var)))
  
  (defun parse-aux-spec (spec)
    "Returns two values: variable and init-form"
;; [&aux {var | (var [init-form])}*])
    (values (if (symbolp spec) spec (first spec))
	    (when (listp spec) (second spec))))

  (defun parse-extended-function (lambda-list body &optional name)
    "Returns two values: the effective arguments and body for a function with
the given lambda-list and body."

;; The lambda list is transformed as follows, since a javascript lambda list is just a 
;; list of variable names, and you have access to the arguments variable inside the function:
;; * standard variables are the mapped directly into the js-lambda list
;; * optional variables' variable names are mapped directly into the lambda list,
;;   and for each optional variable with name v and default value d, a form is produced
;;   (defaultf v d)
;; * when any keyword variables are in the lambda list, a single 'options' variable is
;;   appended to the js-lambda list as the last argument.  WITH-SLOTS is used for all
;;   the variables with  inside the body of the function,
    ;;   a (with-slots ((var-name key-name)) options ...)
    (declare (ignore name))
    (multiple-value-bind (requireds optionals rest? rest keys? keys allow? aux? aux
				    more? more-context more-count key-object)
	(parse-lambda-list lambda-list)
      (declare (ignore allow? aux? aux more? more-context more-count))
      (let* ((options-var (or key-object 'options))
	     ;; optionals are of form (var default-value)
	     (effective-args
	      (remove-if
	       #'null
	       (append requireds
		       (mapcar #'parse-optional-spec optionals)
		       (when keys (list options-var)))))
	     ;; an alist of arg -> default val
	     (initform-pairs
	      (remove
	       nil
	       (append
		;; optional arguments first
		(mapcar #'(lambda (opt-spec)
			    (multiple-value-bind (var val) (parse-optional-spec opt-spec)
			      (cons var val)))
			optionals)
		(if keys? (list (cons options-var '(create))))
		(mapcar #'(lambda (key-spec)
			    (multiple-value-bind (var val x y specified?) (parse-key-spec key-spec)
			      (declare (ignore x y))
			      (when specified? (cons var val))))
			keys))))
	     (body-paren-forms (parse-function-body body)) ;remove documentation
	     ;;
	     (initform-forms
	      (mapcar #'(lambda (default-pair)
			  `(defaultf ,(car default-pair) ,(cdr default-pair)))
		      initform-pairs))
	     (rest-form
	      (if rest?
		  `(defvar ,rest (:.slice (to-array arguments)
				  ,(length effective-args)))
		  `(progn)))
	     (effective-body   (append initform-forms (list rest-form) body-paren-forms))
	     (effective-body
	      (if keys?
		  (list `(with-slots ,(mapcar #'(lambda (key-spec)
						  (multiple-value-bind (var x key-name)
						      (parse-key-spec key-spec)
						    (declare (ignore x))
						    (list var key-name)))
					      keys)
			  ,options-var
			  ,@effective-body))
		  effective-body)))
	(values effective-args effective-body)))))

(ps:defscriptmacro defun (name lambda-list &body body)
  "An extended defun macro that allows cool things like keyword arguments.
lambda-list::=
 (var* 
  [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
  [&rest var] 
  [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
  [&aux {var | (var [init-form])}*])"
  (multiple-value-bind (effective-args effective-body)
      (parse-extended-function lambda-list body name)
    `(%js-defun ,name ,effective-args
      ,@effective-body)))


(ps:defscriptmacro lambda (lambda-list &body body)
  "An extended defun macro that allows cool things like keyword arguments.
lambda-list::=
 (var* 
  [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
  [&rest var] 
  [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
  [&aux {var | (var [init-form])}*])"
  (multiple-value-bind (effective-args effective-body)
      (parse-extended-function lambda-list body)
    `(%js-lambda ,effective-args
      ,@effective-body)))

(defpsmacro defsetf (access-fn lambda-list (store-var) form)
  (setf (find-macro-spec access-fn *script-setf-expanders*)
        (compile nil
                 (let ((var-bindings (set-difference lambda-list lambda-list-keywords)))
                   `(lambda (access-fn-args store-form)
                     (destructuring-bind ,lambda-list
                               access-fn-args
                       (let* ((,store-var (ps:gen-ps-name))
                              (gensymed-names (loop repeat ,(length var-bindings) collecting (ps:gen-ps-name)))
                              (gensymed-arg-bindings (mapcar #'list gensymed-names (list ,@var-bindings))))
                         (destructuring-bind ,var-bindings
                             gensymed-names
                           `(let ((,,store-var ,store-form)
                                  ,@gensymed-arg-bindings)
                             ,,form))))))))
  nil)

(defpsmacro setf (&rest args)
  (flet ((process-setf-clause (place value-form)
           (if (and (listp place) (find-macro-spec (car place) *script-setf-expanders*))
               (funcall (find-macro-spec (car place) *script-setf-expanders*) (cdr place) value-form)
               (let ((exp-place (expand-script-form place)))
                 (if (and (listp exp-place) (find-macro-spec (car exp-place) *script-setf-expanders*))
                     (funcall (find-macro-spec (car exp-place) *script-setf-expanders*) (cdr exp-place) value-form)
                     `(parenscript.javascript::setf1% ,exp-place ,value-form))))))
    (assert (evenp (length args)) ()
            "~s does not have an even number of arguments." (cons 'setf args))
    `(progn ,@(loop for (place value) on args by #'cddr collect (process-setf-clause place value)))))
