(in-package :parenscript)

;;;; The macrology of the Parenscript language.  Special forms and macros.

;;; parenscript gensyms
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
                          `(,symbol (ps-gensym)))))
                  symbols)
     ,@body))

(defun constant-literal-form-p (form)
  (or (numberp form)
      (stringp form)
      (and (listp form)
           (eql 'js-literal (car form)))))

(defpsmacro defaultf (place value)
  `(setf ,place (or (and (=== undefined ,place) ,value)
		 ,place)))

;;; array literals
(defpsmacro list (&rest values)
  `(array ,@values))

(defpsmacro make-array (&rest inits)
  `(new (*array ,@inits)))

;;; slot access
(defpsmacro slot-value (obj &rest slots)
  (if (null (rest slots))
      `(%js-slot-value ,obj ,(first slots))
      `(slot-value (slot-value ,obj ,(first slots)) ,@(rest slots))))

(defpsmacro with-slots (slots object &rest body)
  (flet ((slot-var (slot) (if (listp slot) (first slot) slot))
	 (slot-symbol (slot) (if (listp slot) (second slot) slot)))
    `(symbol-macrolet ,(mapcar #'(lambda (slot)
				   `(,(slot-var slot) (slot-value ,object ',(slot-symbol slot))))
			       slots)
      ,@body)))

(defpsmacro case (value &rest clauses)
  (labels ((make-clause (val body more)
             (cond ((listp val)
                    (append (mapcar #'list (butlast val))
                            (make-clause (first (last val)) body more)))
                   ((member val '(t otherwise))
                    (make-clause 'default body more))
                   (more `((,val ,@body break)))
                   (t `((,val ,@body))))))
    `(switch ,value ,@(mapcon (lambda (clause)
                                (make-clause (car (first clause))
                                             (cdr (first clause))
                                             (rest clause)))
                              clauses))))

(define-ps-special-form let (expecting bindings &rest body)
  (declare (ignore expecting))
  (let ((defvars (mapcar (lambda (binding) (if (atom binding)
                                               `(defvar ,binding)
                                               `(defvar ,@binding)))
                         bindings)))
    (compile-parenscript-form `(progn ,@defvars ,@body))))

;;; iteration
(defpsmacro dotimes (iter &rest body)
  (let ((var (first iter))
        (times (second iter)))
  `(do ((,var 0 (1+ ,var)))
       ((>= ,var ,times))
     ,@body)))

(defpsmacro dolist (i-array &rest body)
  (let ((var (first i-array))
	(array (second i-array))
	(arrvar (ps-gensym "tmp-arr"))
	(idx (ps-gensym "tmp-i")))
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

(define-ps-special-form macrolet (expecting macros &body body)
  (declare (ignore expecting))
  (with-temp-macro-environment (macro-env-dict)
    (dolist (macro macros)
      (destructuring-bind (name arglist &body body)
          macro
	(setf (get-macro-spec name macro-env-dict)
	      (cons nil (make-ps-macro-function arglist body)))))
    (compile-parenscript-form `(progn ,@body))))

(define-ps-special-form symbol-macrolet (expecting symbol-macros &body body)
  (declare (ignore expecting))
  (with-temp-macro-environment (macro-env-dict)
    (dolist (macro symbol-macros)
      (destructuring-bind (name expansion)
          macro
	(setf (get-macro-spec name macro-env-dict)
	      (cons t (make-ps-macro-function () (list `',expansion))))))
    (compile-parenscript-form `(progn ,@body))))

(define-ps-special-form defmacro (expecting name args &body body)
  (declare (ignore expecting))
  (define-script-macro% name args body :symbol-macro-p nil)
  nil)

(define-ps-special-form define-symbol-macro (expecting name expansion)
  (declare (ignore expecting))
  (define-script-macro% name () (list `',expansion) :symbol-macro-p t)
  nil)

(defpsmacro lisp (&body forms)
  "Evaluates the given forms in Common Lisp at ParenScript
macro-expansion time. The value of the last form is treated as a
ParenScript expression and is inserted into the generated Javascript
\(use nil for no-op)."
  (eval (cons 'progn forms)))

(defpsmacro rebind (variables &body body)
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
;; * when any keyword variables are in the lambda list, a single 'optional-args' variable is
;;   appended to the js-lambda list as the last argument.  WITH-SLOTS is used for all
;;   the variables with  inside the body of the function,
    ;;   a (with-slots ((var-name key-name)) optional-args ...)
    (declare (ignore name))
    (multiple-value-bind (requireds optionals rest? rest keys? keys allow? aux? aux
				    more? more-context more-count key-object)
	(parse-lambda-list lambda-list)
      (declare (ignore allow? aux? aux more? more-context more-count))
      (let* ((options-var (or key-object (ps-gensym)))
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
                  (with-ps-gensyms (i)
                    `(progn (defvar ,rest array)
                            (dotimes (,i (- arguments.length ,(length effective-args)))
                              (setf (aref ,rest ,i) (aref arguments (+ ,i ,(length effective-args)))))))
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

(defpsmacro defun (name lambda-list &body body)
  "An extended defun macro that allows cool things like keyword arguments.
lambda-list::=
 (var* 
  [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
  [&rest var] 
  [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
  [&aux {var | (var [init-form])}*])"
  (if (symbolp name)
      `(defun-normal ,name ,lambda-list ,@body)
      (progn (assert (and (= (length name) 2) (eql 'setf (car name))) ()
                     "(defun ~s ~s ...) needs to have a symbol or (setf symbol) for a name." name lambda-list)
             `(defun-setf ,name ,lambda-list ,@body))))

(defpsmacro defun-normal (name lambda-list &body body)
  (multiple-value-bind (effective-args effective-body)
      (parse-extended-function lambda-list body name)
    `(%js-defun ,name ,effective-args
      ,@effective-body)))

(defvar *defun-setf-name-prefix* "__setf_")

(defpsmacro defun-setf (setf-name lambda-list &body body)
  (let ((mangled-function-name (intern (concatenate 'string *defun-setf-name-prefix* (symbol-name (second setf-name)))
                                       (symbol-package (second setf-name))))
        (function-args (cdr (ordered-set-difference lambda-list lambda-list-keywords))))
    `(progn (defsetf ,(second setf-name) ,(cdr lambda-list) (store-var)
              `(,',mangled-function-name ,store-var ,@(list ,@function-args)))
            (defun ,mangled-function-name ,lambda-list ,@body))))

(defpsmacro lambda (lambda-list &body body)
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

(defpsmacro defsetf-long (access-fn lambda-list (store-var) form)
  (setf (get-macro-spec access-fn *script-setf-expanders*)
        (compile nil
                 (let ((var-bindings (ordered-set-difference lambda-list lambda-list-keywords)))
                   `(lambda (access-fn-args store-form)
                     (destructuring-bind ,lambda-list
                               access-fn-args
                       (let* ((,store-var (ps-gensym))
                              (gensymed-names (loop repeat ,(length var-bindings) collecting (ps-gensym)))
                              (gensymed-arg-bindings (mapcar #'list gensymed-names (list ,@var-bindings))))
                         (destructuring-bind ,var-bindings
                             gensymed-names
                           `(let (,@gensymed-arg-bindings
                                  (,,store-var ,store-form))
                             ,,form))))))))
  nil)

(defpsmacro defsetf-short (access-fn update-fn &optional docstring)
  (declare (ignore docstring))
  (setf (get-macro-spec access-fn *script-setf-expanders*)
        (lambda (access-fn-args store-form)
          `(,update-fn ,@access-fn-args ,store-form)))
  nil)

(defpsmacro defsetf (access-fn &rest args)
  `(,(if (= (length args) 3) 'defsetf-long 'defsetf-short) ,access-fn ,@args))

(defpsmacro setf (&rest args)
  (flet ((process-setf-clause (place value-form)
           (if (and (listp place) (get-macro-spec (car place) *script-setf-expanders*))
               (funcall (get-macro-spec (car place) *script-setf-expanders*) (cdr place) value-form)
               (let ((exp-place (ps-macroexpand place)))
                 (if (and (listp exp-place) (get-macro-spec (car exp-place) *script-setf-expanders*))
                     (funcall (get-macro-spec (car exp-place) *script-setf-expanders*) (cdr exp-place) value-form)
                     `(setf1% ,exp-place ,value-form))))))
    (assert (evenp (length args)) ()
            "~s does not have an even number of arguments." (cons 'setf args))
    `(progn ,@(loop for (place value) on args by #'cddr collect (process-setf-clause place value)))))
