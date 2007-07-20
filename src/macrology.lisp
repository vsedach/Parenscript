(in-package :parenscript)

;;;; The macrology of the basic Parenscript language.  Special forms and macros in the
;;;; Parenscript language.

;;; parenscript gensyms
(defvar *gen-script-name-counter* 0)

(defun gen-script-name-string (&key (prefix "_ps_"))
  "Generates a unique valid javascript identifier ()"
  (concatenate 'string
               prefix (princ-to-string (incf *gen-script-name-counter*))))

(defun gen-script-name (&key (prefix "_ps_"))
  "Generate a new javascript identifier."
  (intern (gen-script-name-string :prefix prefix)
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
                          `(,symbol (gen-script-name :prefix ,prefix))
                          `(,symbol (gen-script-name)))))
                  symbols)
     ,@body))

(defvar *var-counter* 0)

(defun script-gensym (&optional (name "js"))
  (intern (format nil "tmp-~A-~A" name (incf *var-counter*)) #.*package*))

;;; literals
(defmacro defscriptliteral (name string)
  "Define a Javascript literal that will expand to STRING."
  `(define-script-special-form ,name () (make-instance 'expression :value ,string)))

(defscriptliteral this      "this")
(defscriptliteral t         "true")
(defscriptliteral nil       "null")
(defscriptliteral false     "false")
(defscriptliteral undefined "undefined")

(defmacro defscriptkeyword (name string)
  "Define a Javascript keyword that will expand to STRING."
  `(define-script-special-form ,name () (make-instance 'statement :value ,string)))

(defscriptkeyword break    "break")
(defscriptkeyword continue "continue")

;;; array literals
(define-script-special-form array (&rest values)
  (make-instance 'array-literal
		 :values (mapcar #'compile-to-expression values)))

(defscriptmacro list (&rest values)
  `(array ,@values))

(define-script-special-form aref (array &rest coords)
  (make-instance 'script-aref
		 :array (compile-to-expression array)
		 :index (mapcar #'compile-to-expression coords)))


(defscriptmacro make-array (&rest inits)
  `(new (*array ,@inits)))

;;; object literals (maps and hash-tables)
(define-script-special-form {} (&rest values)
  (make-instance 'object-literal
                 :values (loop
                            for (key value) on values by #'cddr
                            collect (cons key (compile-to-expression value)))))

;;; operators
(define-script-special-form ++ (x)
  (make-instance 'one-op :pre-p nil :op "++"
		 :value (compile-to-expression x)))

(define-script-special-form -- (x)
  (make-instance 'one-op :pre-p nil :op "--"
		 :value (compile-to-expression x)))

(define-script-special-form incf (x &optional (delta 1))
  (if (eql delta 1)
      (make-instance 'one-op :pre-p t :op "++"
                     :value (compile-to-expression x))
      (make-instance 'op-form
                     :operator '+=
                     :args (mapcar #'compile-to-expression
                                   (list x delta )))))

(define-script-special-form decf (x &optional (delta 1))
  (if (eql delta 1)
      (make-instance 'one-op :pre-p t :op "--"
                     :value (compile-to-expression x))
      (make-instance 'op-form
                     :operator '-=
                     :args (mapcar #'compile-to-expression
                                   (list x delta )))))

(define-script-special-form - (first &rest rest)
  (if (null rest)
      (make-instance 'one-op
                     :pre-p t
                     :op "-"
                     :value (compile-to-expression first))
      (make-instance 'op-form
                     :operator '-
                     :args (mapcar #'compile-to-expression
                                   (cons first rest)))))

(define-script-special-form not (x)
  (let ((value (compile-to-expression x)))
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

(define-script-special-form ~ (x)
  (let ((expr (compile-to-expression x)))
    (make-instance 'one-op :pre-p t :op "~" :value expr)))

;;; progn
(define-script-special-form progn (&rest body)
  (make-instance 'script-body
		 :statements (mapcar #'compile-to-statement body)))

(defmethod expression-precedence ((body script-body))
  (if (= (length (b-statements body)) 1)
      (expression-precedence (first (b-statements body)))
      (op-precedence 'comma)))

;;; function definition
(define-script-special-form lambda (args &rest body)
  (make-instance 'script-lambda
                 :args (mapcar #'compile-to-symbol args)
                 :body (make-instance 'script-body
                                      :indent "  "
                                      :statements (mapcar #'compile-to-statement body))))

(define-script-special-form defun (name args &rest body)
  (make-instance 'script-defun
		 :name (compile-to-symbol name)
		 :args (mapcar #'compile-to-symbol args)
		 :body (make-instance 'script-body
				      :indent "  "
				      :statements (mapcar #'compile-to-statement body))))

;;; object creation
(define-script-special-form create (&rest args)
  (make-instance 'script-object
		 :slots (loop for (name val) on args by #'cddr
			      collect (let ((name-expr (compile-to-expression name)))
					(assert (or (typep name-expr 'script-variable)
						    (typep name-expr 'string-literal)
						    (typep name-expr 'number-literal)))
					(list name-expr (compile-to-expression val))))))


(define-script-special-form slot-value (obj slot)
  (make-instance 'script-slot-value :object (compile-to-expression obj)
   		   :slot (compile-script-form slot)))

;;; cond
(define-script-special-form cond (&rest clauses)
  (make-instance 'script-cond
		 :tests (mapcar (lambda (clause) (compile-to-expression (car clause)))
				clauses)
		 :bodies (mapcar (lambda (clause) (compile-to-body (cons 'progn (cdr clause)) :indent "  "))
				 clauses)))

;;; if
(define-script-special-form if (test then &optional else)
  (make-instance 'script-if :test (compile-to-expression test)
		 :then (compile-to-body then :indent "  ")
		 :else (when else
			 (compile-to-body else :indent "  "))))

(defmethod expression-precedence ((if script-if))
  (op-precedence 'if))

;;; switch
(define-script-special-form switch (value &rest clauses)
  (let ((clauses (mapcar #'(lambda (clause)
			     (let ((val (first clause))
				   (body (cdr clause)))
			       (list (if (eql val 'default)
					 'default
					 (compile-to-expression val))
				     (compile-to-body (cons 'progn body) :indent "  "))))
			 clauses))
	(check (compile-to-expression value)))
    (make-instance 'script-switch :value check
		   :clauses clauses)))


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
	      (t (make-instance 'script-setf :lhs lhs :rhsides (list rhs)))))
      (make-instance 'script-setf :lhs lhs :rhsides (list rhs))))

(define-script-special-form setf (&rest args)
  (let ((assignments (loop for (lhs rhs) on args by #'cddr
			   for rexpr = (compile-to-expression rhs)
			   for lexpr = (compile-to-expression lhs)
			   collect (make-js-test lexpr rexpr))))
    (if (= (length assignments) 1)
	(first assignments)
	(make-instance 'script-body :indent "" :statements assignments))))

(defmethod expression-precedence ((setf script-setf))
  (op-precedence '=))

;;; defvar
(define-script-special-form defvar (name &optional value)
  (make-instance 'script-defvar :names (list (compile-to-symbol name))
		 :value (when value (compile-to-expression value))))

;;; let
(define-script-special-form let (decls &rest body)
  (let ((defvars (mapcar #'(lambda (decl)
			     (if (atom decl)
                                 (make-instance 'script-defvar
                                       :names (list (compile-to-symbol decl))
                                       :value nil)
                                 (let ((name (first decl))
                                       (value (second decl)))
                                   (make-instance 'script-defvar
                                                  :names (list (compile-to-symbol name))
                                                  :value (compile-to-expression value)))))
			 decls)))
    (make-instance 'script-sub-body
		   :indent "  "
		   :statements (nconc defvars
				 (mapcar #'compile-to-statement body)))))

;;; iteration
(defun make-for-vars (decls)
  (loop for decl in decls
	for var = (if (atom decl) decl (first decl))
	for init = (if (atom decl) nil (second decl))
	collect (make-instance 'script-defvar :names (list (compile-to-symbol var))
			       :value (compile-to-expression init))))

(defun make-for-steps (decls)
  (loop for decl in decls
	when (= (length decl) 3)
	collect (compile-to-expression (third decl))))

(define-script-special-form do (decls termination &rest body)
  (let ((vars (make-for-vars decls))
	(steps (make-for-steps decls))
	(check (compile-to-expression (list 'not (first termination))))
	(body (compile-to-body (cons 'progn body) :indent "  ")))
    (make-instance 'script-for
		   :vars vars
		   :steps steps
		   :check check
		   :body body)))

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

(define-script-special-form doeach (decl &rest body)
  (make-instance 'for-each :name (compile-to-symbol (first decl))
		 :value (compile-to-expression (second decl))
		 :body (compile-to-body (cons 'progn body) :indent "  ")))

(define-script-special-form while (check &rest body)
  (make-instance 'script-while
		 :check (compile-to-expression check)
		 :body (compile-to-body (cons 'progn body) :indent "  ")))

;;; with
(define-script-special-form with (statement &rest body)
  (make-instance 'script-with
		 :obj (compile-to-expression statement)
		 :body (compile-to-body (cons 'progn body) :indent "  ")))


;;; try-catch
(define-script-special-form try (body &rest clauses)
  (let ((body (compile-to-body body :indent "  "))
	(catch (cdr (assoc :catch clauses)))
	(finally (cdr (assoc :finally clauses))))
    (assert (not (cdar catch)) nil "Sorry, currently only simple catch forms are supported.")
    (make-instance 'script-try
		   :body body
		   :catch (when catch (list (compile-to-symbol (caar catch))
					    (compile-to-body (cons 'progn (cdr catch))
								:indent "  ")))
		   :finally (when finally (compile-to-body (cons 'progn finally)
							      :indent "  ")))))
;;; regex
(define-script-special-form regex (regex)
  (make-instance 'regex :value (string regex)))

;;; TODO instanceof
(define-script-special-form instanceof (value type)
  (make-instance 'script-instanceof
                 :value (compile-to-expression value)
                 :type (compile-to-expression type)))

;;; script packages
(define-script-special-form blank-statement ()
  (make-instance 'blank-statement))

(defscriptmacro defpackage (name &rest options)
  "Defines a Parenscript package."
  (labels ((opt-name (opt) (if (listp opt) (car opt) opt)))
  (let ((nicknames nil) (lisp-package nil) (secondary-lisp-packages nil)
	(exports nil) (used-packages nil) (documentation nil))
    (dolist (opt options)
      (case (opt-name opt)
	  (:nicknames (setf nicknames (rest opt)))
	  (:secondary-lisp-packages secondary-lisp-packages t)
	  (:export (setf exports (rest opt)))
	  (:use (setf used-packages (rest opt)))
	  (:documentation (setf documentation (second opt)))))
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
  (setf (comp-env-current-package
	 *compilation-environment*)
	(comp-env-find-package *compilation-environment* package-designator))
  `(progn))

;;; single operations
(defmacro define-parse-script-single-op (name &optional (superclass 'expression))
  (let ((script-name (intern (concatenate 'string "SCRIPT-" (symbol-name name)) #.*package*)))
    `(define-script-special-form ,name (value)
       (make-instance ',script-name :value (compile-to-expression value)))
    ))

(define-parse-script-single-op return statement)
(define-parse-script-single-op throw statement)
(define-parse-script-single-op delete)
(define-parse-script-single-op void)
(define-parse-script-single-op typeof)
(define-parse-script-single-op new)

;;; conditional compilation
(define-script-special-form cc-if (test &rest body)
  (make-instance 'cc-if :test test
		 :body (mapcar #'compile-script-form body)))

;;; standard macros
(defscriptmacro with-slots (slots object &rest body)
  `(symbol-macrolet ,(mapcar #'(lambda (slot)
				 `(,slot '(slot-value ,object ',slot)))
			     slots)
    ,@body))

(defscriptmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defscriptmacro unless (test &rest body)
  `(if (not ,test) (progn ,@body)))

(defscriptmacro 1- (form)
  `(- ,form 1))

(defscriptmacro 1+ (form)
  `(+ ,form 1))

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
(use nil for no-op)."
  (eval (cons 'progn forms)))


(defscriptmacro rebind (variables expression)
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

;;; Math library
(defscriptmacro floor (expr)
  `(*Math.floor ,expr))

(defscriptmacro random ()
  `(*Math.random))

(defscriptmacro evenp (num)
  `(= (% ,num 2) 0))

(defscriptmacro oddp (num)
  `(= (% ,num 2) 1))

;;; helper macros
(define-script-special-form js (&rest body)
  (make-instance 'string-literal
		 :value (string-join (js-to-statement-strings
				      (compile-script-form (cons 'progn body)) 0) " ")))

(define-script-special-form script-inline (&rest body)
  (make-instance 'string-literal
		 :value (concatenate
			 'string
			 "javascript:"
			 (string-join (js-to-statement-strings
				       (compile-script-form (cons 'progn body)) 0) " "))))
(defscriptmacro js-inline (&rest body)
  `(script-inline ,@body))

;;; dual lisp/parenscript macro balderdash
;;; TODO: should probably move elsewhere ;;;
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
    (defscriptmacro ,name ,args ,@body)))

(defun import-macros-from-lisp (&rest names)
  "Import the named Lisp macros into the ParenScript macro environment."
  (dolist (name names)
    (let ((name name))
      (undefine-js-special-form name)
      (setf (get-macro-spec name *script-macro-toplevel*)
            (cons nil (lambda (&rest args)
                        (macroexpand `(,name ,@args))))))))

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
    (string-join (js-to-statement-strings (compile-script-form (list 'progn ,@body)) 0) " ")))
