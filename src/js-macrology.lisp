(in-package :parenscript)

;;;; The macrology of the basic Javascript-in-SEXPs language.  Special forms and macros.

;;; literals
(defmacro defpsliteral (name string)
  `(define-ps-special-form ,name (expecting)
    (declare (ignore expecting))
    (list 'js-literal ,string)))

(defpsliteral this      "this")
(defpsliteral t         "true")
(defpsliteral true      "true")
(defpsliteral false     "false")
(defpsliteral f         "false")
(defpsliteral nil       "null")
(defpsliteral undefined "undefined")

(defmacro defpskeyword (name string)
  `(define-ps-special-form ,name (expecting)
    (declare (ignore expecting))
    (list 'js-keyword ,string)))

(defpskeyword break    "break")
(defpskeyword continue "continue")

(define-ps-special-form array (expecting &rest values)
  (declare (ignore expecting))
  (cons 'array-literal (mapcar (lambda (form) (compile-parenscript-form form :expecting :expression))
                               values)))

(define-ps-special-form aref (expecting array &rest coords)
  (declare (ignore expecting))
  (list 'js-aref (compile-parenscript-form array :expecting :expression)
        (mapcar (lambda (form)
                  (compile-parenscript-form form :expecting :expression))
                coords)))

(define-ps-special-form {} (expecting &rest arrows)
  (declare (ignore expecting))
  (cons 'object-literal (loop for (key value) on arrows by #'cddr
                              collect (cons key (compile-parenscript-form value :expecting :expression)))))

;;; operators
(define-ps-special-form incf (expecting x &optional (delta 1))
  (declare (ignore expecting))
  (if (equal delta 1)
      (list 'unary-operator "++" (compile-parenscript-form x :expecting :expression) :prefix t)
      (list 'operator '+= (list (compile-parenscript-form x :expecting :expression)
                                (compile-parenscript-form delta :expecting :expression)))))

(define-ps-special-form decf (expecting x &optional (delta 1))
  (declare (ignore expecting))
  (if (equal delta 1)
      (list 'unary-operator "--" (compile-parenscript-form x :expecting :expression) :prefix t)
      (list 'operator '-= (list (compile-parenscript-form x :expecting :expression)
                                (compile-parenscript-form delta :expecting :expression)))))

(define-ps-special-form - (expecting first &rest rest)
  (declare (ignore expecting))
  (if (null rest)
      (list 'unary-operator "-" (compile-parenscript-form first :expecting :expression) :prefix t)
      (list 'operator '- (mapcar (lambda (val) (compile-parenscript-form val :expecting :expression))
                                 (cons first rest)))))

(define-ps-special-form not (expecting x)
  (declare (ignore expecting))
  (let ((form (compile-parenscript-form x :expecting :expression))
        (not-op nil))
    (if (and (eql (first form) 'operator)
	     (= (length (third form)) 2)
             (setf not-op (case (second form)
                            (== '!=)
                            (< '>=)
                            (> '<=)
                            (<= '>)
                            (>= '<)
                            (!= '==)
                            (=== '!==)
                            (!== '===)
                            (t nil))))
        (list 'operator not-op (third form))
        (list 'unary-operator "!" form :prefix t))))

(define-ps-special-form ~ (expecting x)
  (declare (ignore expecting))
  (list 'unary-operator "~" (compile-parenscript-form x :expecting :expressin) :prefix t))

(defun flatten-blocks (body)
  (when body
    (if (and (listp (car body))
             (eql 'js-block (caar body)))
        (append (third (car body)) (flatten-blocks (cdr body)))
        (cons (car body) (flatten-blocks (cdr body))))))

(define-ps-special-form progn (expecting &rest body)
  (if (and (eql expecting :expression) (= 1 (length body)))
      (compile-parenscript-form (car body) :expecting :expression)
      (list 'js-block
            (if (eql expecting :statement) t nil)
            (let* ((block (mapcar (lambda (form)
                                    (compile-parenscript-form form :expecting :statement))
                                  body))
                   (clean-block (remove nil block))
                   (flat-block (flatten-blocks clean-block))
                   (reachable-block (append (remove-if #'constant-literal-form-p (butlast flat-block))
                                            (last flat-block))))
              reachable-block))))

;;; function definition
(define-ps-special-form %js-lambda (expecting args &rest body)
  (declare (ignore expecting))
  (list 'js-lambda (mapcar (lambda (arg)
                             (compile-parenscript-form arg :expecting :symbol))
                           args)
        (compile-parenscript-form `(progn ,@body))))

(define-ps-special-form %js-defun (expecting name args &rest body)
  (declare (ignore expecting))
  (list 'js-defun name
        (mapcar (lambda (val) (compile-parenscript-form val :expecting :symbol)) args)
	(compile-parenscript-form `(progn ,@body))))

;;; object creation
(define-ps-special-form create (expecting &rest args)
  (declare (ignore expecting))
  (list 'js-object (loop for (name val) on args by #'cddr collecting
                         (let ((name-expr (compile-parenscript-form name :expecting :expression)))
                           (assert (or (stringp name-expr)
                                       (numberp name-expr)
                                       (and (listp name-expr)
                                            (or (eql 'js-variable (car name-expr))
                                                (eql 'script-quote (car name-expr)))))
                                   ()
                                   "Slot ~s is not one of js-variable, keyword, string or number." name-expr)
                           (list name-expr (compile-parenscript-form val :expecting :expression))))))

(define-ps-special-form %js-slot-value (expecting obj slot)
  (declare (ignore expecting))
  (if (ps::ps-macroexpand slot)
      (list 'js-slot-value (compile-parenscript-form obj :expecting :expression) (compile-parenscript-form slot))
      (compile-parenscript-form obj :expecting :expression)))

(define-ps-special-form cond (expecting &rest clauses)
  (ecase expecting
    (:statement (list 'js-cond-statement
                      (mapcar (lambda (clause)
                                (destructuring-bind (test &rest body)
                                    clause
                                  (list (compile-parenscript-form test :expecting :expression)
                                        (compile-parenscript-form `(progn ,@body)))))
                              clauses)))
    (:expression (make-cond-clauses-into-nested-ifs clauses))))

(defun make-cond-clauses-into-nested-ifs (clauses)
  (if clauses
      (destructuring-bind (test &rest body)
          (car clauses)
        (if (eq t test)
            (compile-parenscript-form `(progn ,@body) :expecting :expression)
            (list 'js-expression-if (compile-parenscript-form test :expecting :expression)
                  (compile-parenscript-form `(progn ,@body) :expecting :expression)
                  (make-cond-clauses-into-nested-ifs (cdr clauses)))))
      (compile-parenscript-form nil :expecting :expression)))

(define-ps-special-form if (expecting test then &optional else)
  (ecase expecting
    (:statement (list 'js-statement-if (compile-parenscript-form test :expecting :expression)
                      (compile-parenscript-form `(progn ,then))
                      (when else (compile-parenscript-form `(progn ,else)))))
    (:expression (list 'js-expression-if (compile-parenscript-form test :expecting :expression)
                       (compile-parenscript-form then :expecting :expression)
                       (compile-parenscript-form else :expecting :expression)))))

(define-ps-special-form switch (expecting test-expr &rest clauses)
  (declare (ignore expecting))
  (let ((clauses (mapcar (lambda (clause)
			     (let ((val (car clause))
				   (body (cdr clause)))
			       (list (if (eql val 'default)
					 'default
					 (compile-parenscript-form val :expecting :expression))
                                     (compile-parenscript-form `(progn ,@body)))))
			 clauses))
	(expr (compile-parenscript-form test-expr :expecting :expression)))
    (list 'js-switch expr clauses)))

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

(defun smart-setf (lhs rhs)
  (if (and (listp rhs)
           (eql 'operator (car rhs))
	   (member lhs (third rhs) :test #'equalp))
      (let ((args-without (remove lhs (third rhs) :count 1 :test #'equalp))
	    (args-without-first (remove lhs (third rhs) :count 1 :end 1 :test #'equalp)))
	(cond ((and (equal (car args-without) 1) (eql (second rhs) '+))
	       (list 'unary-operator "++" lhs :prefix nil))
	      ((and (equal (second args-without-first) 1) (eql (second rhs) '-))
	       (list 'unary-operator "--" lhs :prefix nil))
	      ((and (assignment-op (second rhs))
		    (member (second rhs) '(+ *))
                    (equalp lhs (first (third rhs))))
	       (list 'operator (assignment-op (second rhs))
                     (list lhs (list 'operator (second rhs) args-without-first))))
	      ((and (assignment-op (second rhs)) (equalp (first (third rhs)) lhs))
	       (list 'operator (assignment-op (second rhs))
                     (list lhs (list 'operator (second rhs) (cdr (third rhs))))))
	      (t (list 'js-assign lhs rhs))))
      (list 'js-assign lhs rhs)))

(define-ps-special-form setf1% (expecting lhs rhs)
  (declare (ignore expecting))
  (smart-setf (compile-parenscript-form lhs :expecting :expression) (compile-parenscript-form rhs :expecting :expression)))

(define-ps-special-form defvar (expecting name &rest value)
  (declare (ignore expecting))
  (append (list 'js-defvar name)
          (when value
            (assert (= (length value) 1) () "Wrong number of arguments to defvar: ~s" `(defvar ,name ,@value))
           (list (compile-parenscript-form (car value) :expecting :expression)))))

;;; iteration
(defun make-for-vars (decls)
  (loop for decl in decls
	for var = (if (atom decl) decl (first decl))
	for init-value = (if (atom decl) nil (second decl))
	collect (cons (compile-parenscript-form var :expecting :symbol) (compile-parenscript-form init-value))))

(defun make-for-steps (decls)
  (loop for decl in decls
	when (= (length decl) 3)
	collect (compile-parenscript-form (third decl) :expecting :expression)))

(define-ps-special-form do (expecting decls termination-test &rest body)
  (declare (ignore expecting))
  (let ((vars (make-for-vars decls))
	(steps (make-for-steps decls))
	(test (compile-parenscript-form `(not ,(first termination-test)) :expecting :expression))
	(body (compile-parenscript-form `(progn ,@body))))
    (list 'js-for vars steps test body)))

(define-ps-special-form doeach (expecting decl &rest body)
  (declare (ignore expecting))
  (list 'js-for-each
        (first decl)
        (compile-parenscript-form (second decl) :expecting :expression)
	(compile-parenscript-form `(progn ,@body))))

(define-ps-special-form while (expecting test &rest body)
  (declare (ignore expecting))
  (list 'js-while (compile-parenscript-form test :expecting :expression)
                  (compile-parenscript-form `(progn ,@body))))

(define-ps-special-form with (expecting expression &rest body)
  (declare (ignore expecting))
  (list 'js-with (compile-parenscript-form expression :expecting :expression)
		 (compile-parenscript-form `(progn ,@body))))

(define-ps-special-form try (expecting form &rest clauses)
  (declare (ignore expecting))
  (let ((catch (cdr (assoc :catch clauses)))
        (finally (cdr (assoc :finally clauses))))
    (assert (not (cdar catch)) nil "Sorry, currently only simple catch forms are supported.")
    (assert (or catch finally) ()
            "Try form should have either a catch or a finally clause or both.")
    (list 'js-try (compile-parenscript-form `(progn ,form))
          :catch (when catch (list (compile-parenscript-form (caar catch) :expecting :symbol)
                                   (compile-parenscript-form `(progn ,@(cdr catch)))))
          :finally (when finally (compile-parenscript-form `(progn ,@finally))))))

(define-ps-special-form regex (expecting regex)
  (declare (ignore expecting))
  (list 'js-regex (string regex)))

;;; TODO instanceof
(define-ps-special-form instanceof (expecting value type)
  (declare (ignore expecting))
  (list 'js-instanceof (compile-parenscript-form value :expecting :expression)
        (compile-parenscript-form type :expecting :expression)))

;;; single operations
(mapcar (lambda (op) (eval `(define-ps-special-form ,op (expecting value)
                             (declare (ignore expecting))
                             (list 'js-named-operator ',op (compile-parenscript-form value)))))
        '(throw delete void typeof new))

(define-ps-special-form return (expecting &optional value)
  (declare (ignore expecting))
  (list 'js-return (compile-parenscript-form value :expecting :expression)))

;;; conditional compilation
(define-ps-special-form cc-if (expecting test &rest body)
  (declare (ignore expecting))
  (list 'cc-if test (mapcar #'compile-parenscript-form body)))

;;; standard macros
(defpsmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defpsmacro unless (test &rest body)
  `(if (not ,test) (progn ,@body)))

(defpsmacro 1- (form)
  `(- ,form 1))

(defpsmacro 1+ (form)
  `(+ ,form 1))
