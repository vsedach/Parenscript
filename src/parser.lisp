(in-package :parenscript)

;;; special forms

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *js-special-forms* (make-hash-table :test 'equal)
        "A hash-table containing functions that implement ParenScript
special forms, indexed by name (a string).")

  (defun undefine-js-special-form (name)
    (when (gethash (symbol-name name) *js-special-forms*)
      (warn "Redefining ParenScript special form ~S" name)
      (remhash (symbol-name name) *js-special-forms*))))

(defmacro define-js-special-form (name lambda-list &rest body)
  "Define a special form NAME. Arguments are destructured according to
LAMBDA-LIST. The resulting JS language types are appended to the
ongoing javascript compilation."
  (let ((js-name (intern (concatenate 'string "JS-" (symbol-name name)) #.*package*))
        (arglist (gensym "ps-arglist-")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defun ,js-name (&rest ,arglist)
        (destructuring-bind ,lambda-list
            ,arglist
          ,@body))
      (setf (gethash ,(symbol-name name) *js-special-forms*) #',js-name))))

(defun js-special-form-p (form)
  (and (consp form)
       (symbolp (car form))
       (gethash (symbol-name (car form)) *js-special-forms*)))

(defun js-get-special-form (name)
  (when (symbolp name)
    (gethash (symbol-name name) *js-special-forms*)))

;;; macro expansion

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-macro-env-dictionary ()
    (make-hash-table :test 'equal))
  
  (defvar *js-macro-toplevel* (make-macro-env-dictionary)
    "Toplevel macro environment dictionary. Key is symbol-name of the macro, value is (symbol-macro-p . expansion-function).")
  (defvar *js-macro-env* (list *js-macro-toplevel*)
    "Current macro environment."))

(defmacro get-macro-spec (name env-dict)
  `(gethash (symbol-name ,name) ,env-dict))

(defun lookup-macro-spec (name &optional (environment *js-macro-env*))
  (when (symbolp name)
    (do ((env environment (cdr env)))
        ((null env) nil)
      (let ((val (get-macro-spec name (car env))))
        (when val
          (return-from lookup-macro-spec
            (values val (or (cdr env)
                            (list *js-macro-toplevel*)))))))))

(defun symbol-macro-p (name &optional (environment *js-macro-env*))
  (and (symbolp name) (car (lookup-macro-spec name environment))))

(defun macro-p (name &optional (environment *js-macro-env*))
  (and (symbolp name) (let ((macro-spec (lookup-macro-spec name environment)))
                        (and macro-spec (not (car macro-spec))))))

(defun lookup-macro-expansion-function (name &optional (environment *js-macro-env*))
  "Lookup NAME in the given macro expansion environment (which
defaults to the current macro environment). Returns the expansion
function and the parent macro environment of the macro."
  (multiple-value-bind (macro-spec parent-env)
      (lookup-macro-spec name environment)
    (values (cdr macro-spec) parent-env)))

(defmacro defjsmacro (name args &rest body)
  "Define a ParenScript macro, and store it in the toplevel ParenScript macro environment."
  (let ((lambda-list (gensym "ps-lambda-list-"))
        (body (if (stringp (first body)) (rest body) body))) ;; drop docstring
    (undefine-js-special-form name)
    `(setf (get-macro-spec ',name *js-macro-toplevel*)
      (cons nil (lambda (&rest ,lambda-list)
                  (destructuring-bind ,args
                      ,lambda-list
                    ,@body))))))

(defmacro defmacro/js (name args &body body)
  "Define a Lisp macro and import it into the ParenScript macro environment."
  `(progn (defmacro ,name ,args ,@body)
	  (js:import-macros-from-lisp ',name)))

(defmacro defmacro+js (name args &body body)
  "Define a Lisp macro and a ParenScript macro in their respective
macro environments. This function should be used when you want to use
the same macro in both Lisp and ParenScript, but the 'macroexpand' of
that macro in Lisp makes the Lisp macro unsuitable to be imported into
the ParenScript macro environment."
  `(progn (defmacro ,name ,args ,@body)
          (js:defjsmacro ,name ,args ,@body)))

(defun import-macros-from-lisp (&rest names)
  "Import the named Lisp macros into the ParenScript macro environment."
  (dolist (name names)
    (let ((name name))
      (undefine-js-special-form name)
      (setf (get-macro-spec name *js-macro-toplevel*)
            (cons nil (lambda (&rest args)
                        (macroexpand `(,name ,@args))))))))

(defun js-expand-form (expr)
  (if (consp expr)
      (let ((op (car expr))
            (args (cdr expr)))
        (cond ((equal op 'quote) expr)
              ((macro-p op) (multiple-value-bind (expansion-function macro-env)
                                (lookup-macro-expansion-function op)
                              (js-expand-form (let ((*js-macro-env* macro-env))
                                                (apply expansion-function args)))))
              (t expr)))
      (cond ((js-special-form-p expr) expr)
            ((symbol-macro-p expr) (multiple-value-bind (expansion-function macro-env)
                                       (lookup-macro-expansion-function expr)
                                     (js-expand-form (let ((*js-macro-env* macro-env))
                                                       (funcall expansion-function)))))
            (t expr))))

(defvar *gen-js-name-counter* 0)

(defun gen-js-name-string (&key (prefix "_ps_"))
  "Generates a unique valid javascript identifier ()"
  (concatenate 'string
               prefix (princ-to-string (incf *gen-js-name-counter*))))

(defun gen-js-name (&key (prefix "_ps_"))
  "Generate a new javascript identifier."
  (intern (gen-js-name-string :prefix prefix)
          (find-package :js)))

(defmacro with-unique-js-names (symbols &body body)
  "Evaluate BODY with the variables on SYMBOLS bound to new javascript identifiers.

Each element of SYMBOLS is either a symbol or a list of (symbol
prefix)."
  `(let* ,(mapcar (lambda (symbol)
                    (destructuring-bind (symbol &optional prefix)
                        (if (consp symbol)
                            symbol
                            (list symbol))
                      (if prefix
                          `(,symbol (gen-js-name :prefix ,prefix))
                          `(,symbol (gen-js-name)))))
                  symbols)
     ,@body))

(defjsmacro rebind (variables expression)
  "Creates a new js lexical environment and copies the given
  variable(s) there.  Executes the body in the new environment. This
  has the same effect as a new (let () ...) form in lisp but works on
  the js side for js closures."
  (unless (listp variables)
    (setf variables (list variables)))
  `((lambda ()
      (let ((new-context (new *object)))
        ,@(loop for variable in variables
                do (setf variable (symbol-to-js variable))
                collect `(setf (slot-value new-context ,variable) (slot-value this ,variable)))
        (with new-context
              (return ,expression))))))

(defvar *var-counter* 0)

(defun js-gensym (&optional (name "js"))
  (intern (format nil "tmp-~A-~A" name (incf *var-counter*)) #.*package*))

;;; reserved Javascript keywords

(defvar *reserved-javascript-keywords*
  '("abstract" "else" "instanceof" "switch" "boolean" "enum" "int" "synchronized"
    "break" "export" "interface" "this" "byte" "extends" "long" "throw" "case"
    "native" "throws" "catch" "final" "new" "transient" "char" "finally" "float"
    "package" "try" "const" "for" "private" "typeof" "continue" "function"
    "protected" "var" "debugger" "goto" "public" "void" "default" "if" "return"
    "volatile" "delete" "implements" "short" "while" "do" "import" "static" "with"
    "double" "in" "super" "class"))

(defun reserved-identifier-p (id-string)
  (find id-string *reserved-javascript-keywords* :test #'string-equal))

(defmethod initialize-instance :after ((var js-variable) &rest initargs)
  (declare (ignore initargs))
  (when (reserved-identifier-p (slot-value var 'value))
    (warn "~a is a reserved Javascript keyword and should not be used as a variable or function name." (slot-value var 'value))))

;;; literals

(defmacro defjsliteral (name string)
  "Define a Javascript literal that will expand to STRING."
  `(define-js-special-form ,name () (make-instance 'expression :value ,string)))

(defjsliteral this      "this")
(defjsliteral t         "true")
(defjsliteral nil       "null")
(defjsliteral false     "false")
(defjsliteral undefined "undefined")

(defmacro defjskeyword (name string)
  "Define a Javascript keyword that will expand to STRING."
  `(define-js-special-form ,name () (make-instance 'statement :value ,string)))

(defjskeyword break    "break")
(defjskeyword continue "continue")

;;; array literals

(define-js-special-form array (&rest values)
  (make-instance 'array-literal
		 :values (mapcar #'js-compile-to-expression values)))

(defjsmacro list (&rest values)
  `(array ,@values))

(define-js-special-form aref (array &rest coords)
  (make-instance 'js-aref
		 :array (js-compile-to-expression array)
		 :index (mapcar #'js-compile-to-expression coords)))


(defjsmacro make-array (&rest inits)
  `(new (*array ,@inits)))

;;; object literals (maps and hash-tables)

(define-js-special-form {} (&rest values)
  (make-instance 'object-literal
                 :values (loop
                            for (key value) on values by #'cddr
                            collect (cons key (js-compile-to-expression value)))))

;;; operators
(define-js-special-form ++ (x)
  (make-instance 'one-op :pre-p nil :op "++"
		 :value (js-compile-to-expression x)))

(define-js-special-form -- (x)
  (make-instance 'one-op :pre-p nil :op "--"
		 :value (js-compile-to-expression x)))

(define-js-special-form incf (x &optional (delta 1))
  (if (eql delta 1)
      (make-instance 'one-op :pre-p t :op "++"
                     :value (js-compile-to-expression x))
      (make-instance 'op-form
                     :operator '+=
                     :args (mapcar #'js-compile-to-expression
                                   (list x delta )))))

(define-js-special-form decf (x &optional (delta 1))
  (if (eql delta 1)
      (make-instance 'one-op :pre-p t :op "--"
                     :value (js-compile-to-expression x))
      (make-instance 'op-form
                     :operator '-=
                     :args (mapcar #'js-compile-to-expression
                                   (list x delta )))))

(define-js-special-form - (first &rest rest)
  (if (null rest)
      (make-instance 'one-op
                     :pre-p t
                     :op "-"
                     :value (js-compile-to-expression first))
      (make-instance 'op-form
                     :operator '-
                     :args (mapcar #'js-compile-to-expression
                                   (cons first rest)))))

(define-js-special-form not (x)
  (let ((value (js-compile-to-expression x)))
    (if (and (typep value 'op-form)
	     (= (length (op-args value)) 2))
	(let ((new-op (case (operator value)
			(== '!=)
			(< '>=)
			(> '<=)
			(<= '>)
			(>= '<)
			(!= '==)
			(=== '!==)
			(!== '===)
			(t nil))))
	  (if new-op
	      (make-instance 'op-form :operator new-op
			     :args (op-args value))
	      (make-instance 'one-op :pre-p t :op "!"
			    :value value)))
	(make-instance 'one-op :pre-p t :op "!"
		       :value value))))

(define-js-special-form ~ (x)
  (let ((expr (js-compile-to-expression x)))
    (make-instance 'one-op :pre-p t :op "~" :value expr)))

;;; function calls

(defun funcall-form-p (form)
  (and (listp form)
       (not (op-form-p form))
       (not (js-special-form-p form))))

(defun method-call-p (form)
  (and (funcall-form-p form)
       (symbolp (first form))
       (eql (char (symbol-name (first form)) 0) #\.)))

;;; progn

(define-js-special-form progn (&rest body)
  (make-instance 'js-body
		 :stmts (mapcar #'js-compile-to-statement body)))

(defmethod expression-precedence ((body js-body))
  (if (= (length (b-stmts body)) 1)
      (expression-precedence (first (b-stmts body)))
      (op-precedence 'comma)))

;;; function definition
(define-js-special-form lambda (args &rest body)
  (make-instance 'js-lambda
                 :args (mapcar #'js-compile-to-symbol args)
                 :body (make-instance 'js-body
                                      :indent "  "
                                      :stmts (mapcar #'js-compile-to-statement body))))

(define-js-special-form defun (name args &rest body)
  (make-instance 'js-defun
		 :name (js-compile-to-symbol name)
		 :args (mapcar #'js-compile-to-symbol args)
		 :body (make-instance 'js-body
				      :indent "  "
				      :stmts (mapcar #'js-compile-to-statement body))))

;;; object creation
(define-js-special-form create (&rest args)
  (make-instance 'js-object
		 :slots (loop for (name val) on args by #'cddr
			      collect (let ((name-expr (js-compile-to-expression name)))
					(assert (or (typep name-expr 'js-variable)
						    (typep name-expr 'string-literal)
						    (typep name-expr 'number-literal)))
					(list name-expr (js-compile-to-expression val))))))


(define-js-special-form slot-value (obj slot)
  (make-instance 'js-slot-value :object (js-compile-to-expression obj)
   		   :slot (js-compile slot)))

;;; cond
(define-js-special-form cond (&rest clauses)
  (make-instance 'js-cond
		 :tests (mapcar (lambda (clause) (js-compile-to-expression (car clause)))
				clauses)
		 :bodies (mapcar (lambda (clause) (js-compile-to-body (cons 'progn (cdr clause)) :indent "  "))
				 clauses)))

;;; if
(define-js-special-form if (test then &optional else)
  (make-instance 'js-if :test (js-compile-to-expression test)
		 :then (js-compile-to-body then :indent "  ")
		 :else (when else
			 (js-compile-to-body else :indent "  "))))

(defmethod expression-precedence ((if js-if))
  (op-precedence 'if))

;;; switch
(define-js-special-form switch (value &rest clauses)
  (let ((clauses (mapcar #'(lambda (clause)
			     (let ((val (first clause))
				   (body (cdr clause)))
			       (list (if (eql val 'default)
					 'default
					 (js-compile-to-expression val))
				     (js-compile-to-body (cons 'progn body) :indent "  "))))
			 clauses))
	(check (js-compile-to-expression value)))
    (make-instance 'js-switch :value check
		   :clauses clauses)))


(defjsmacro case (value &rest clauses)
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

;;; assignment
(defun assignment-op (op)
  (case op
    (+ '+=)
    (~ '~=)
    (\& '\&=)
    (\| '\|=)
    (- '-=)
    (* '*=)
    (% '%=)
    (>> '>>=)
    (^  '^=)
    (<< '<<=)
    (>>> '>>>=)
    (/   '/=)
    (t   nil)))

(defun make-js-test (lhs rhs)
  (if (and (typep rhs 'op-form)
	   (member lhs (op-args rhs) :test #'js-equal))
      (let ((args-without (remove lhs (op-args rhs)
				  :count 1 :test #'js-equal))
	    (args-without-first (remove lhs (op-args rhs)
					:count 1 :end 1
					:test #'js-equal))
	    (one (list (make-instance 'number-literal :value 1))))
	#+nil
	(format t "OPERATOR: ~S, ARGS-WITHOUT: ~S, ARGS-WITHOUT-FIRST ~S~%"
		(operator rhs)
		args-without
		args-without-first)
	(cond ((and (js-equal args-without one)
		    (eql (operator rhs) '+))
	       (make-instance 'one-op :pre-p nil :op "++"
			      :value lhs))
	      ((and (js-equal args-without-first one)
		    (eql (operator rhs) '-))
	       (make-instance 'one-op :pre-p nil :op "--"
			      :value lhs))
	      ((and (assignment-op (operator rhs))
		    (member (operator rhs)
			    '(+ *))
                    (js-equal lhs (first (op-args rhs))))
	       (make-instance 'op-form
			      :operator (assignment-op (operator rhs))
			      :args (list lhs (make-instance 'op-form
							     :operator (operator rhs)
							     :args args-without-first))))
	      ((and (assignment-op (operator rhs))
		    (js-equal (first (op-args rhs)) lhs))
	       (make-instance 'op-form
			      :operator (assignment-op (operator rhs))
			      :args (list lhs (make-instance 'op-form
							     :operator (operator rhs)
							     :args (cdr (op-args rhs))))))
	      (t (make-instance 'js-setf :lhs lhs :rhsides (list rhs)))))
      (make-instance 'js-setf :lhs lhs :rhsides (list rhs))))

(define-js-special-form setf (&rest args)
  (let ((assignments (loop for (lhs rhs) on args by #'cddr
			   for rexpr = (js-compile-to-expression rhs)
			   for lexpr = (js-compile-to-expression lhs)
			   collect (make-js-test lexpr rexpr))))
    (if (= (length assignments) 1)
	(first assignments)
	(make-instance 'js-body :indent "" :stmts assignments))))

(defmethod expression-precedence ((setf js-setf))
  (op-precedence '=))

;;; defvar
(define-js-special-form defvar (name &optional value)
  (make-instance 'js-defvar :names (list (js-compile-to-symbol name))
		 :value (when value (js-compile-to-expression value))))

;;; let
(define-js-special-form let (decls &rest body)
  (let ((defvars (mapcar #'(lambda (decl)
			     (if (atom decl)
                                 (make-instance 'js-defvar
                                       :names (list (js-compile-to-symbol decl))
                                       :value nil)
                                 (let ((name (first decl))
                                       (value (second decl)))
                                   (make-instance 'js-defvar
                                                  :names (list (js-compile-to-symbol name))
                                                  :value (js-compile-to-expression value)))))
			 decls)))
    (make-instance 'js-sub-body
		   :indent "  "
		   :stmts (nconc defvars
				 (mapcar #'js-compile-to-statement body)))))

;;; iteration
(defun make-for-vars (decls)
  (loop for decl in decls
	for var = (if (atom decl) decl (first decl))
	for init = (if (atom decl) nil (second decl))
	collect (make-instance 'js-defvar :names (list (js-compile-to-symbol var))
			       :value (js-compile-to-expression init))))

(defun make-for-steps (decls)
  (loop for decl in decls
	when (= (length decl) 3)
	collect (js-compile-to-expression (third decl))))

(define-js-special-form do (decls termination &rest body)
  (let ((vars (make-for-vars decls))
	(steps (make-for-steps decls))
	(check (js-compile-to-expression (list 'not (first termination))))
	(body (js-compile-to-body (cons 'progn body) :indent "  ")))
    (make-instance 'js-for
		   :vars vars
		   :steps steps
		   :check check
		   :body body)))

(defjsmacro dotimes (iter &rest body)
  (let ((var (first iter))
        (times (second iter)))
  `(do ((,var 0 (1+ ,var)))
       ((>= ,var ,times))
     ,@body)))

(defjsmacro dolist (i-array &rest body)
  (let ((var (first i-array))
	(array (second i-array))
	(arrvar (js-gensym "arr"))
	(idx (js-gensym "i")))
    `(let ((,arrvar ,array))
      (do ((,idx 0 (1+ ,idx)))
	  ((>= ,idx (slot-value ,arrvar 'length)))
	(let ((,var (aref ,arrvar ,idx)))
	  ,@body)))))

(define-js-special-form doeach (decl &rest body)
  (make-instance 'for-each :name (js-compile-to-symbol (first decl))
		 :value (js-compile-to-expression (second decl))
		 :body (js-compile-to-body (cons 'progn body) :indent "  ")))

(define-js-special-form while (check &rest body)
  (make-instance 'js-while
		 :check (js-compile-to-expression check)
		 :body (js-compile-to-body (cons 'progn body) :indent "  ")))

;;; with

;;; try-catch
(define-js-special-form try (body &rest clauses)
  (let ((body (js-compile-to-body body :indent "  "))
	(catch (cdr (assoc :catch clauses)))
	(finally (cdr (assoc :finally clauses))))
    (assert (not (cdar catch)) nil "Sorry, currently only simple catch forms are supported.")
    (make-instance 'js-try
		   :body body
		   :catch (when catch (list (js-compile-to-symbol (caar catch))
					    (js-compile-to-body (cons 'progn (cdr catch))
								:indent "  ")))
		   :finally (when finally (js-compile-to-body (cons 'progn finally)
							      :indent "  ")))))
;;; regex
(define-js-special-form regex (regex)
  (make-instance 'regex :value (string regex)))

;;; TODO instanceof
(define-js-special-form instanceof (value type)
  (make-instance 'js-instanceof
                 :value (js-compile-to-expression value)
                 :type (js-compile-to-expression type)))

;;; single operations
(defmacro define-parse-js-single-op (name &optional (superclass 'expression))
  (let ((js-name (intern (concatenate 'string "JS-" (symbol-name name)) #.*package*)))
    `(define-js-special-form ,name (value)
       (make-instance ',js-name :value (js-compile-to-expression value)))
    ))

(define-parse-js-single-op return statement)
(define-parse-js-single-op throw statement)
(define-parse-js-single-op delete)
(define-parse-js-single-op void)
(define-parse-js-single-op typeof)
(define-parse-js-single-op new)

;;; conditional compilation
(define-js-special-form cc-if (test &rest body)
  (make-instance 'cc-if :test test
		 :body (mapcar #'js-compile body)))

;;; standard macros
(defjsmacro with-slots (slots object &rest body)
  `(symbol-macrolet ,(mapcar #'(lambda (slot)
				 `(,slot '(slot-value ,object ',slot)))
			     slots)
    ,@body))

(defjsmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defjsmacro unless (test &rest body)
  `(if (not ,test) (progn ,@body)))

(defjsmacro 1- (form)
  `(- ,form 1))

(defjsmacro 1+ (form)
  `(+ ,form 1))

;;; macros
(defmacro with-temp-macro-environment ((var) &body body)
  `(let* ((,var (make-macro-env-dictionary))
          (*js-macro-env* (cons ,var *js-macro-env*)))
    ,@body))

(define-js-special-form macrolet (macros &body body)
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
    (js-compile `(progn ,@body))))

(define-js-special-form symbol-macrolet (symbol-macros &body body)
  (with-temp-macro-environment (macro-env-dict)
    (dolist (macro symbol-macros)
      (destructuring-bind (name &body expansion)
          macro
	(setf (get-macro-spec name macro-env-dict)
	      (cons t (compile nil `(lambda () ,@expansion))))))
    (js-compile `(progn ,@body))))

(defjsmacro defmacro (name args &body body)
  `(lisp (defjsmacro ,name ,args ,@body) nil))

(defjsmacro lisp (&body forms)
  "Evaluates the given forms in Common Lisp at ParenScript
macro-expansion time. The value of the last form is treated as a
ParenScript expression and is inserted into the generated Javascript
(use nil for no-op)."
  (eval (cons 'progn forms)))

;;; Math library
(defjsmacro floor (expr)
  `(*Math.floor ,expr))

(defjsmacro random ()
  `(*Math.random))

(defjsmacro evenp (num)
  `(= (% ,num 2) 0))

(defjsmacro oddp (num)
  `(= (% ,num 2) 1))

;;; helper macros
(define-js-special-form js (&rest body)
  (make-instance 'string-literal
		 :value (string-join (js-to-statement-strings
				      (js-compile (cons 'progn body)) 0) " ")))

(define-js-special-form js-inline (&rest body)
  (make-instance 'string-literal
		 :value (concatenate
			 'string
			 "javascript:"
			 (string-join (js-to-statement-strings
				       (js-compile (cons 'progn body)) 0) " "))))

;;;; compiler interface ;;;;
(defun js-compile (form)
  (setf form (js-expand-form form))
  (cond ((stringp form)
	 (make-instance 'string-literal :value form))
        ((characterp form)
	 (make-instance 'string-literal :value (string form)))
	((numberp form)
	 (make-instance 'number-literal :value form))
	((symbolp form)
	 (let ((c-macro (js-get-special-form form)))
	   (if c-macro
	       (funcall c-macro)
	       (make-instance 'js-variable :value form))))
	((and (consp form)
	      (eql (first form) 'quote))
	 (make-instance 'js-quote :value (second form)))
	((consp form)
	 (js-compile-list form))
	(t (error "Unknown atomar expression ~S" form))))

(defun js-compile-list (form)
  (let* ((name (car form))
	 (args (cdr form))
	 (js-form (js-get-special-form name)))
    (cond (js-form
	   (apply js-form args))

	  ((op-form-p form)
	   (make-instance 'op-form
			  :operator (js-convert-op-name (js-compile-to-symbol (first form)))
			  :args (mapcar #'js-compile-to-expression (rest form))))

	  ((method-call-p form)
	   (make-instance 'method-call
			  :method (js-compile-to-symbol (first form))
			  :object (js-compile-to-expression (second form))
			  :args (mapcar #'js-compile-to-expression (cddr form))))

	  ((funcall-form-p form)
	   (make-instance 'function-call
			  :function (js-compile-to-expression (first form))
			  :args (mapcar #'js-compile-to-expression (rest form))))

	  (t (error "Unknown form ~S" form)))))

(defun js-compile-to-expression (form)
  (let ((res (js-compile form)))
    (assert (typep res 'expression))
    res))

(defun js-compile-to-symbol (form)
  (let ((res (js-compile form)))
    (when (typep res 'js-variable)
      (setf res (value res)))
    (assert (symbolp res) ()
            "~a is expected to be a symbol, but compiles to ~a. This could be due to ~a being a special form." form res form)
    res))

(defun js-compile-to-statement (form)
  (let ((res (js-compile form)))
    (assert (typep res 'statement))
    res))

(defun js-compile-to-body (form &key (indent ""))
  (let ((res (js-compile-to-statement form)))
    (if (typep res 'js-body)
	(progn (setf (b-indent res) indent)
	       res)
	(make-instance 'js-body
		       :indent indent
		       :stmts (list res)))))

(defmacro js (&rest body)
  `(js* '(progn ,@body)))

(defmacro js* (&rest body)
  "Return the javascript string representing BODY.

Body is evaluated."
  `(string-join
    (js-to-statement-strings (js-compile (list 'progn ,@body)) 0)
    (string #\Newline)))

(defun js-to-string (expr)
  (string-join
   (js-to-statement-strings (js-compile expr) 0)
   (string #\Newline)))

(defun js-to-line (expr)
  (string-join
   (js-to-statement-strings (js-compile expr) 0) " "))

(defmacro js-file (&rest body)
  `(html
    (:princ
     (js ,@body))))

(defmacro js-script (&rest body)
  `((:script :type "text/javascript")
    (:princ (format nil "~%// <![CDATA[~%"))
    (:princ (js ,@body))
    (:princ (format nil "~%// ]]>~%"))))

(defmacro js-inline (&rest body)
  `(js-inline* '(progn ,@body)))

(defmacro js-inline* (&rest body)
  "Just like JS-INLINE except that BODY is evaluated before being
converted to javascript."
  `(concatenate 'string "javascript:"
    (string-join (js-to-statement-strings (js-compile (list 'progn ,@body)) 0) " ")))


