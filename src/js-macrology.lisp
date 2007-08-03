(in-package :parenscript.javascript)

;;;; The macrology of the basic Javascript-in-SEXPs language.  Special forms and macros.

;;; literals
(defmacro defscriptliteral (name string)
  "Define a Javascript literal that will expand to STRING."
  `(define-script-special-form ,name () (make-instance 'expression :value ,string)))

(defscriptliteral this      "this")
(defscriptliteral t         "true")
(defscriptliteral true      "true")
(defscriptliteral false     "false")
(defscriptliteral f         "false")
(defscriptliteral nil       "null")
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

(define-script-special-form aref (array &rest coords)
  (make-instance 'js-aref
		 :array (compile-to-expression array)
		 :index (mapcar #'compile-to-expression coords)))


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

(define-script-special-form progn (&rest body)
  (make-instance 'js-block :statements (mapcar #'compile-to-statement body)))

(defmethod expression-precedence ((body js-block))
  (if (= (length (block-statements body)) 1)
      (expression-precedence (first (block-statements body)))
      (op-precedence 'comma)))

;;; function definition
(define-script-special-form %js-lambda (args &rest body)
  (make-instance 'js-lambda
                 :args (mapcar #'compile-to-symbol args)
                 :body (make-instance 'js-block
                                      :indent "  "
                                      :statements (mapcar #'compile-to-statement body))))

(define-script-special-form %js-defun (name args &rest body)
  (make-instance 'js-defun
		 :name (compile-to-symbol name)
		 :args (mapcar #'compile-to-symbol args)
		 :body (make-instance 'js-block
				      :indent "  "
				      :statements (mapcar #'compile-to-statement body))))

;;; object creation
(define-script-special-form create (&rest args)
  (make-instance 'js-object
		 :slots (loop for (name val) on args by #'cddr
			      collect (let ((name-expr (compile-to-expression name)))
					(assert (or (typep name-expr 'js-variable)
						    (typep name-expr 'script-quote)
						    (typep name-expr 'string-literal)
						    (typep name-expr 'number-literal)))
					(list name-expr (compile-to-expression val))))))


(define-script-special-form %js-slot-value (obj slot)
  (if (ps::expand-script-form slot)
      (make-instance 'js-slot-value
		     :object (compile-to-expression obj)
                     :slot (compile-script-form slot))
      (compile-to-expression obj)))

;;; cond
(define-script-special-form cond (&rest clauses)
  (make-instance 'js-cond
		 :tests (mapcar (lambda (clause) (compile-to-expression (car clause)))
				clauses)
		 :bodies (mapcar (lambda (clause) (compile-to-block (cons 'progn (cdr clause)) :indent "  "))
				 clauses)))

;;; if
(define-script-special-form if (test then &optional else)
  (make-instance 'js-if :test (compile-to-expression test)
		 :then (compile-to-block then :indent "  ")
		 :else (when else
			 (compile-to-block else :indent "  "))))

(defmethod expression-precedence ((if js-if))
  (op-precedence 'if))

;;; switch
(define-script-special-form switch (value &rest clauses)
  (let ((clauses (mapcar #'(lambda (clause)
			     (let ((val (first clause))
				   (body (cdr clause)))
			       (list (if (eql val 'default)
					 'default
					 (compile-to-expression val))
				     (compile-to-block (cons 'progn body) :indent "  "))))
			 clauses))
	(check (compile-to-expression value)))
    (make-instance 'js-switch :value check
		   :clauses clauses)))


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
	   (member lhs (op-args rhs) :test #'script-equal))
      (let ((args-without (remove lhs (op-args rhs)
				  :count 1 :test #'script-equal))
	    (args-without-first (remove lhs (op-args rhs)
					:count 1 :end 1
					:test #'script-equal))
	    (one (list (make-instance 'number-literal :value 1))))
	#+nil
	(format t "OPERATOR: ~S, ARGS-WITHOUT: ~S, ARGS-WITHOUT-FIRST ~S~%"
		(operator rhs)
		args-without
		args-without-first)
	(cond ((and (script-equal args-without one)
		    (eql (operator rhs) '+))
	       (make-instance 'one-op :pre-p nil :op "++"
			      :value lhs))
	      ((and (script-equal args-without-first one)
		    (eql (operator rhs) '-))
	       (make-instance 'one-op :pre-p nil :op "--"
			      :value lhs))
	      ((and (assignment-op (operator rhs))
		    (member (operator rhs)
			    '(+ *))
                    (script-equal lhs (first (op-args rhs))))
	       (make-instance 'op-form
			      :operator (assignment-op (operator rhs))
			      :args (list lhs (make-instance 'op-form
							     :operator (operator rhs)
							     :args args-without-first))))
	      ((and (assignment-op (operator rhs))
		    (script-equal (first (op-args rhs)) lhs))
	       (make-instance 'op-form
			      :operator (assignment-op (operator rhs))
			      :args (list lhs (make-instance 'op-form
							     :operator (operator rhs)
							     :args (cdr (op-args rhs))))))
	      (t (make-instance 'js-setf :lhs lhs :rhsides (list rhs)))))
      (make-instance 'js-setf :lhs lhs :rhsides (list rhs))))

(define-script-special-form setf1% (lhs rhs)
  (make-js-test (compile-to-expression lhs) (compile-to-expression rhs)))

(defmethod expression-precedence ((setf js-setf))
  (op-precedence '=))

;;; defvar
(define-script-special-form defvar (name &optional value)
  (make-instance 'js-defvar :names (list (compile-to-symbol name))
		 :value (when value (compile-to-expression value))))

;;; iteration
(defun make-for-vars (decls)
  (loop for decl in decls
	for var = (if (atom decl) decl (first decl))
	for init = (if (atom decl) nil (second decl))
	collect (make-instance 'js-defvar :names (list (compile-to-symbol var))
			       :value (compile-to-expression init))))

(defun make-for-steps (decls)
  (loop for decl in decls
	when (= (length decl) 3)
	collect (compile-to-expression (third decl))))

(define-script-special-form do (decls termination &rest body)
  (let ((vars (make-for-vars decls))
	(steps (make-for-steps decls))
	(check (compile-to-expression (list 'not (first termination))))
	(body (compile-to-block (cons 'progn body) :indent "  ")))
    (make-instance 'js-for
		   :vars vars
		   :steps steps
		   :check check
		   :body body)))

(define-script-special-form doeach (decl &rest body)
  (make-instance 'for-each :name (compile-to-symbol (first decl))
		 :value (compile-to-expression (second decl))
		 :body (compile-to-block (cons 'progn body) :indent "  ")))

(define-script-special-form while (check &rest body)
  (make-instance 'js-while
		 :check (compile-to-expression check)
		 :body (compile-to-block (cons 'progn body) :indent "  ")))

;;; with
(define-script-special-form with (statement &rest body)
  (make-instance 'js-with
		 :obj (compile-to-expression statement)
		 :body (compile-to-block (cons 'progn body) :indent "  ")))


;;; try-catch
(define-script-special-form try (body &rest clauses)
  (let ((body (compile-to-block body :indent "  "))
	(catch (cdr (assoc :catch clauses)))
	(finally (cdr (assoc :finally clauses))))
    (assert (not (cdar catch)) nil "Sorry, currently only simple catch forms are supported.")
    (make-instance 'js-try
		   :body body
		   :catch (when catch (list (compile-to-symbol (caar catch))
					    (compile-to-block (cons 'progn (cdr catch))
								:indent "  ")))
		   :finally (when finally (compile-to-block (cons 'progn finally)
							      :indent "  ")))))
;;; regex
(define-script-special-form regex (regex)
  (make-instance 'regex :value (string regex)))

;;; TODO instanceof
(define-script-special-form instanceof (value type)
  (make-instance 'js-instanceof
                 :value (compile-to-expression value)
                 :type (compile-to-expression type)))

;;; single operations
(defmacro define-parse-script-single-op (name &optional (superclass 'expression))
  (let ((script-name (intern (concatenate 'string "JS-" (symbol-name name)) #.*package*)))
    `(define-script-special-form ,name (value)
       (make-instance ',script-name :value (compile-to-expression value)))
    ))

(define-parse-script-single-op throw statement)
(define-parse-script-single-op delete)
(define-parse-script-single-op void)
(define-parse-script-single-op typeof)
(define-parse-script-single-op new)

(define-script-special-form return (&optional value)
  (make-instance 'js-return :value (compile-to-expression value)))

;;; conditional compilation
(define-script-special-form cc-if (test &rest body)
  (make-instance 'cc-if :test test
		 :body (mapcar #'compile-script-form body)))

;;; standard macros
(defscriptmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defscriptmacro unless (test &rest body)
  `(if (not ,test) (progn ,@body)))

(defscriptmacro 1- (form)
  `(- ,form 1))

(defscriptmacro 1+ (form)
  `(+ ,form 1))

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
(defscriptmacro parenscript::js-inline (&rest body)
  `(script-inline ,@body))
