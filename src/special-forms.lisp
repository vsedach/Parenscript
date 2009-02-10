(in-package :parenscript)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; literals
(defmacro defpsliteral (name string)
  `(progn
     (add-ps-literal ',name)
     (define-ps-special-form ,name (expecting)
       (declare (ignore expecting))
       (list 'js-literal ,string))))

(defpsliteral this      "this")
(defpsliteral t         "true")
(defpsliteral true      "true")
(defpsliteral false     "false")
(defpsliteral f         "false")
(defpsliteral nil       "null")
(defpsliteral undefined "undefined")

(macrolet ((def-for-literal (name printer)
             `(progn
                (add-ps-literal ',name)
                (define-ps-special-form ,name (expecting &optional label)
                  (declare (ignore expecting))
                  (list ',printer label)))))
  (def-for-literal break js-break)
  (def-for-literal continue js-continue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unary operators
(macrolet ((def-unary-ops (&rest ops)
             `(progn ,@(mapcar (lambda (op)
                                 (let ((op (if (listp op) (car op) op))
                                       (spacep (if (listp op) (second op) nil)))
                                   `(define-ps-special-form ,op (expecting x)
                                      (declare (ignore expecting))
                                      (list 'unary-operator ',op
                                            (compile-parenscript-form x :expecting :expression)
                                            :prefix t :space ,spacep))))
                               ops))))
  (def-unary-ops ~ ! (new t) (delete t) (void t) (typeof t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; statements
(define-ps-special-form return (expecting &optional value)
  (declare (ignore expecting))
  (list 'js-return (compile-parenscript-form value :expecting :expression)))

(define-ps-special-form throw (expecting value)
  (declare (ignore expecting))
  (list 'js-throw (compile-parenscript-form value :expecting :expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; arrays
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

(defpsmacro list (&rest values)
  `(array ,@values))

(defpsmacro make-array (&rest initial-values)
  `(new (*array ,@initial-values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operators
(define-ps-special-form incf (expecting x &optional (delta 1))
  (declare (ignore expecting))
  (if (equal delta 1)
      (list 'unary-operator '++ (compile-parenscript-form x :expecting :expression) :prefix t)
      (list 'operator '+= (list (compile-parenscript-form x :expecting :expression)
                                (compile-parenscript-form delta :expecting :expression)))))

(define-ps-special-form decf (expecting x &optional (delta 1))
  (declare (ignore expecting))
  (if (equal delta 1)
      (list 'unary-operator '-- (compile-parenscript-form x :expecting :expression) :prefix t)
      (list 'operator '-= (list (compile-parenscript-form x :expecting :expression)
                                (compile-parenscript-form delta :expecting :expression)))))

(define-ps-special-form - (expecting first &rest rest)
  (declare (ignore expecting))
  (if (null rest)
      (list 'unary-operator '- (compile-parenscript-form first :expecting :expression) :prefix t)
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
        (list 'unary-operator '! form :prefix t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; control structures
(defun flatten-blocks (body)
  (when body
    (if (and (listp (car body))
             (eql 'js-block (caar body)))
        (append (third (car body)) (flatten-blocks (cdr body)))
        (cons (car body) (flatten-blocks (cdr body))))))

(defun constant-literal-form-p (form)
  (or (numberp form)
      (stringp form)
      (and (listp form)
           (eql 'js-literal (car form)))))

(define-ps-special-form progn (expecting &rest body)
  (if (and (eql expecting :expression) (= 1 (length body)))
      (compile-parenscript-form (car body) :expecting :expression)
      (list 'js-block
            expecting
            (let* ((block (mapcar (lambda (form)
                                    (compile-parenscript-form form :expecting expecting))
                                  body))
                   (clean-block (remove nil block))
                   (flat-block (flatten-blocks clean-block))
                   (reachable-block (append (remove-if #'constant-literal-form-p (butlast flat-block))
                                            (last flat-block))))
              reachable-block))))

(define-ps-special-form cond (expecting &rest clauses)
  (ecase expecting
    (:statement (list 'js-cond-statement
                      (mapcar (lambda (clause)
                                (destructuring-bind (test &rest body)
                                    clause
                                  (list (compile-parenscript-form test :expecting :expression)
                                        (compile-parenscript-form `(progn ,@body) :expecting :statement))))
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
                               (cons (if (and (symbolp val)
                                              (eq (ensure-ps-symbol val) 'default))
                                         'default
                                         (compile-parenscript-form val :expecting :expression))
                                     (mapcar (lambda (statement) (compile-parenscript-form statement :expecting :statement))
                                             body))))
                         clauses))
        (expr (compile-parenscript-form test-expr :expecting :expression)))
    (list 'js-switch expr clauses)))

(defpsmacro case (value &rest clauses)
  (labels ((make-clause (val body more)
             (cond ((and (listp val) (not (eq (car val) 'quote)))
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

(defpsmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defpsmacro unless (test &rest body)
  `(if (not ,test) (progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function definition
(defun compile-function-definition (args body)
  (list (mapcar (lambda (arg) (compile-parenscript-form arg :expecting :symbol)) args)
        (let ((*enclosing-lexical-block-declarations* ()))
          ;; the first compilation will produce a list of variables we need to declare in the function body
          (compile-parenscript-form `(progn ,@body) :expecting :statement)
          ;; now declare and compile
          (compile-parenscript-form `(progn ,@(loop for var in *enclosing-lexical-block-declarations* collect `(var ,var))
                                      ,@body) :expecting :statement))))

(define-ps-special-form %js-lambda (expecting args &rest body)
  (declare (ignore expecting))
  (cons 'js-lambda (compile-function-definition args body)))

(define-ps-special-form %js-defun (expecting name args &rest body)
  (declare (ignore expecting))
  (append (list 'js-defun name) (compile-function-definition args body)))

(defun parse-function-body (body)
  (let* ((docstring
          (when (stringp (first body))
            (first body)))
         (body-forms (if docstring (rest body) body)))
    (values body-forms docstring)))

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

(defpsmacro defaultf (place value)
  `(when (=== ,place undefined)
     (setf ,place ,value)))

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
           (body-paren-forms (parse-function-body body)) ; remove documentation
           (initform-forms
            (mapcar #'(lambda (default-pair)
                        `(defaultf ,(car default-pair) ,(cdr default-pair)))
                    initform-pairs))
           (rest-form
            (if rest?
                (with-ps-gensyms (i)
                  `(progn (var ,rest (array))
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
      (values effective-args effective-body))))

(defpsmacro defun (name lambda-list &body body)
  "An extended defun macro that allows cool things like keyword arguments.
lambda-list::=
 (var* 
  [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
  [&rest var] 
  [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
  [&aux {var | (var [init-form])}*])"
  (if (symbolp name)
      `(defun-function ,name ,lambda-list ,@body)
      (progn (assert (and (= (length name) 2) (eq 'setf (ensure-ps-symbol (car name)))) ()
                     "(defun ~s ~s ...) needs to have a symbol or (setf symbol) for a name." name lambda-list)
             `(defun-setf ,name ,lambda-list ,@body))))

(defpsmacro defun-function (name lambda-list &body body)
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

(defpsmacro flet (fn-defs &rest body)
  (flet ((process-fn-def (def)
           `(var ,(car def) (lambda ,@(cdr def)))))
    `(progn ,@(mapcar #'process-fn-def fn-defs) ,@body)))

(defpsmacro labels (fn-defs &rest body)
  (flet ((process-fn-def (def)
           `(var ,(car def) (defun ,(car def) ,@(cdr def)))))
    `(progn ,@(mapcar #'process-fn-def fn-defs) ,@body)))

(defpsmacro defsetf-long (access-fn lambda-list (store-var) form)
  (setf (get-macro-spec access-fn *ps-setf-expanders*)
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
                           `(let* (,@gensymed-arg-bindings
                                   (,,store-var ,store-form))
                             ,,form))))))))
  nil)

(defpsmacro defsetf-short (access-fn update-fn &optional docstring)
  (declare (ignore docstring))
  (setf (get-macro-spec access-fn *ps-setf-expanders*)
        (lambda (access-fn-args store-form)
          `(,update-fn ,@access-fn-args ,store-form)))
  nil)

(defpsmacro defsetf (access-fn &rest args)
  `(,(if (= (length args) 3) 'defsetf-long 'defsetf-short) ,access-fn ,@args))

(defpsmacro funcall (&rest arg-form)
  arg-form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macros
(defmacro with-temp-macro-environment ((var) &body body)
  `(let* ((,var (make-macro-env-dictionary))
          (*ps-macro-env* (cons ,var *ps-macro-env*)))
    ,@body))

(define-ps-special-form macrolet (expecting macros &body body)
  (declare (ignore expecting))
  (with-temp-macro-environment (macro-env-dict)
    (dolist (macro macros)
      (destructuring-bind (name arglist &body body)
          macro
        (setf (get-macro-spec name macro-env-dict)
              (cons nil (eval (make-ps-macro-function arglist body))))))
    (compile-parenscript-form `(progn ,@body))))

(define-ps-special-form symbol-macrolet (expecting symbol-macros &body body)
  (declare (ignore expecting))
  (with-temp-macro-environment (macro-env-dict)
    (dolist (macro symbol-macros)
      (destructuring-bind (name expansion)
          macro
        (setf (get-macro-spec name macro-env-dict)
              (cons t (lambda () expansion)))))
    (compile-parenscript-form `(progn ,@body))))

(define-ps-special-form defmacro (expecting name args &body body)
  (declare (ignore expecting))
  (eval `(defpsmacro ,name ,args ,@body))
  nil)

(define-ps-special-form define-symbol-macro (expecting name expansion)
  (declare (ignore expecting))
  (eval `(define-ps-symbol-macro ,name ,expansion))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; objects
(add-ps-literal '{})
(define-ps-symbol-macro {} (create))

(define-ps-special-form create (expecting &rest arrows)
  (declare (ignore expecting))
  (list 'js-object (loop for (key-expr val-expr) on arrows by #'cddr collecting
                         (let ((key (compile-parenscript-form key-expr :expecting :expression)))
                           (when (keywordp key)
                             (setf key (list 'js-variable key)))
                           (assert (or (stringp key)
                                       (numberp key)
                                       (and (listp key)
                                            (or (eq 'js-variable (car key))
                                                (eq 'ps-quote (car key)))))
                                   ()
                                   "Slot key ~s is not one of js-variable, keyword, string or number." key)
                           (cons key (compile-parenscript-form val-expr :expecting :expression))))))

(define-ps-special-form %js-slot-value (expecting obj slot)
  (declare (ignore expecting))
  (if (ps::ps-macroexpand slot)
      (list 'js-slot-value (compile-parenscript-form obj :expecting :expression) (compile-parenscript-form slot))
      (compile-parenscript-form obj :expecting :expression)))

(define-ps-special-form instanceof (expecting value type)
  (declare (ignore expecting))
  (list 'js-instanceof (compile-parenscript-form value :expecting :expression)
        (compile-parenscript-form type :expecting :expression)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assignment and binding
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
      (let ((args-without-first (remove lhs (third rhs) :count 1 :end 1 :test #'equalp)))
        (cond ((and (assignment-op (second rhs))
                    (member (second rhs) '(+ *))
                    (equalp lhs (first (third rhs))))
               (list 'operator (assignment-op (second rhs))
                     (list lhs (list 'operator (second rhs) args-without-first))))
              (t (list 'js-assign lhs rhs))))
      (list 'js-assign lhs rhs)))

(define-ps-special-form setf1% (expecting lhs rhs)
  (declare (ignore expecting))
  (smart-setf (compile-parenscript-form lhs :expecting :expression) (compile-parenscript-form rhs :expecting :expression)))

(defpsmacro setf (&rest args)
  (flet ((process-setf-clause (place value-form)
           (if (and (listp place) (get-macro-spec (car place) *ps-setf-expanders*))
               (funcall (get-macro-spec (car place) *ps-setf-expanders*) (cdr place) value-form)
               (let ((exp-place (ps-macroexpand place)))
                 (if (and (listp exp-place) (get-macro-spec (car exp-place) *ps-setf-expanders*))
                     (funcall (get-macro-spec (car exp-place) *ps-setf-expanders*) (cdr exp-place) value-form)
                     `(setf1% ,exp-place ,value-form))))))
    (assert (evenp (length args)) ()
            "~s does not have an even number of arguments." (cons 'setf args))
    `(progn ,@(loop for (place value) on args by #'cddr collect (process-setf-clause place value)))))

(defpsmacro psetf (&rest args)
  (let ((vars (loop for x in args by #'cddr collect x))
        (vals (loop for x in (cdr args) by #'cddr collect x)))
    (let ((gensyms (mapcar (lambda (x) (declare (ignore x)) (ps-gensym)) vars)))
      `(simple-let* ,(mapcar #'list gensyms vals)
         (setf ,@(mapcan #'list vars gensyms))))))

(defun check-setq-args (args)
  (let ((vars (loop for x in args by #'cddr collect x)))
    (let ((non-var (find-if (complement #'symbolp) vars)))
      (when non-var
        (error 'type-error :datum non-var :expected-type 'symbol)))))

(defpsmacro setq (&rest args)
  (check-setq-args args)
  `(setf ,@args))

(defpsmacro psetq (&rest args)
  (check-setq-args args)
  `(psetf ,@args))

(define-ps-special-form var (expecting name &rest value)
  (declare (ignore expecting))
  (append (list 'js-var name)
          (when value
            (assert (= (length value) 1) () "Wrong number of arguments to var: ~s" `(var ,name ,@value))
            (list (compile-parenscript-form (car value) :expecting :expression)))))

(defpsmacro defvar (name &rest value)
  "Note: this must be used as a top-level form, otherwise the result will be undefined behavior."
  (pushnew name *ps-special-variables*)
  (assert (or (null value) (= (length value) 1)) () "Wrong number of arguments to defvar: ~s" `(defvar ,name ,@value))
  `(var ,name ,@value))

(defun make-let-vars (bindings)
  (mapcar (lambda (x) (if (listp x) (car x) x)) bindings))

(defun make-let-vals (bindings)
  (mapcar (lambda (x) (if (or (atom x) (endp (cdr x))) nil (second x))) bindings))

(defpsmacro lexical-let* (bindings &body body)
  `((lambda ()
      (let* ,bindings
        ,@body))))

(defpsmacro lexical-let (bindings &body body)
  `((lambda ,(make-let-vars bindings)
      ,@body)
    ,@(make-let-vals bindings)))

(defpsmacro simple-let* (bindings &body body)
  (if bindings
      (let ((var (if (listp (car bindings)) (caar bindings) (car bindings))))
        `(,(if (member var *ps-special-variables*) 'let1-dynamic 'let1) ,(car bindings)
           (simple-let* ,(cdr bindings) ,@body)))
      `(progn ,@body)))

(defpsmacro simple-let (bindings &body body)
  (let ((vars (mapcar (lambda (x) (if (atom x) x (first x))) bindings))
        (vals (mapcar (lambda (x) (if (or (atom x) (endp (cdr x))) nil (second x))) bindings)))
    (let ((gensyms (mapcar (lambda (x) (ps-gensym (format nil "_js_~a" x))) vars)))
      `(simple-let* ,(mapcar #'list gensyms vals)
         (simple-let* ,(mapcar #'list vars gensyms)
           ,@(mapcar (lambda (x) `(delete ,x)) gensyms)
           ,@body)))))

(defpsmacro let* (bindings &body body)
  `(simple-let* ,bindings ,@body))

(defpsmacro let (bindings &body body)
  `(,(if (= 1 (length bindings)) 'simple-let* 'simple-let) ,bindings ,@body))

(define-ps-special-form let1 (expecting binding &rest body)
  (ecase expecting
    (:statement
     (compile-parenscript-form `(progn ,(if (atom binding) `(var ,binding) `(var ,@binding)) ,@body) :expecting :statement))
    (:expression
     (let ((var (if (atom binding) binding (car binding)))
           (variable-assignment (when (listp binding) (cons 'setf binding))))
       (push var *enclosing-lexical-block-declarations*)
       (compile-parenscript-form `(progn ,variable-assignment ,@body) :expecting :expression)))))

(defpsmacro let1-dynamic ((var value) &rest body)
  (with-ps-gensyms (temp-stack-var)
    `(progn (var ,temp-stack-var)
      (try (progn (setf ,temp-stack-var ,var)
                  (setf ,var ,value)
                  ,@body)
       (:finally
        (setf ,var ,temp-stack-var)
        (delete ,temp-stack-var))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iteration
(defun make-for-vars/inits (init-forms)
  (mapcar (lambda (x)
            (cons (compile-parenscript-form (if (atom x) x (first x)) :expecting :symbol)
                  (compile-parenscript-form (if (atom x) nil (second x)) :expecting :expression)))
          init-forms))

(define-ps-special-form labeled-for (expecting label init-forms cond-forms step-forms &rest body)
  (declare (ignore expecting))
  (let ((vars (make-for-vars/inits init-forms))
        (steps (mapcar (lambda (x) (compile-parenscript-form x :expecting :expression)) step-forms))
        (tests (mapcar (lambda (x) (compile-parenscript-form x :expecting :expression)) cond-forms))
        (body (compile-parenscript-form `(progn ,@body))))
    (list 'js-for label vars tests steps body)))

(defpsmacro for (init-forms cond-forms step-forms &body body)
  `(labeled-for nil ,init-forms ,cond-forms ,step-forms ,@body))

(defun do-make-let-bindings (decls)
  (mapcar (lambda (x)
            (if (atom x) x
                (if (endp (cdr x)) (list (car x))
                    (subseq x 0 2))))
          decls))

(defun do-make-init-vars (decls)
  (mapcar (lambda (x) (if (atom x) x (first x))) decls))

(defun do-make-init-vals (decls)
  (mapcar (lambda (x) (if (or (atom x) (endp (cdr x))) nil (second x))) decls))

(defun do-make-for-vars/init (decls)
  (mapcar (lambda (x)
            (if (atom x) x
                (if (endp (cdr x)) x
                    (subseq x 0 2))))
          decls))

(defun do-make-for-steps (decls)
  (mapcar (lambda (x)
            `(setf ,(first x) ,(third x)))
          (remove-if (lambda (x) (or (atom x) (< (length x) 3))) decls)))

(defun do-make-iter-psteps (decls)
  `(psetq
    ,@(mapcan (lambda (x) (list (first x) (third x)))
              (remove-if (lambda (x) (or (atom x) (< (length x) 3))) decls))))

(defpsmacro do* (decls (termination &optional (result nil result?)) &body body)
  (if result?
      `((lambda ()
          (for ,(do-make-for-vars/init decls) ((not ,termination)) ,(do-make-for-steps decls)
               ,@body)
          (return ,result)))
      `(progn
         (for ,(do-make-for-vars/init decls) ((not ,termination)) ,(do-make-for-steps decls)
              ,@body))))

(defpsmacro do (decls (termination &optional (result nil result?)) &body body)
  (if result?
      `((lambda ,(do-make-init-vars decls)
          (for () ((not ,termination)) ()
               ,@body
               ,(do-make-iter-psteps decls))
          (return ,result))
        ,@(do-make-init-vals decls))
      `(let ,(do-make-let-bindings decls)
         (for () ((not ,termination)) ()
              ,@body
              ,(do-make-iter-psteps decls)))))

(define-ps-special-form for-in (expecting decl &rest body)
  (declare (ignore expecting))
  (list 'js-for-in
        (compile-parenscript-form (first decl) :expecting :expression)
        (compile-parenscript-form (second decl) :expecting :expression)
        (compile-parenscript-form `(progn ,@body))))

(defpsmacro doeach ((var array &optional (result (values) result?)) &body body)
  "Iterates over `array'.  If `var' is a symbol, binds `var' to each
element key.  If `var' is a list, it must be a list of two
symbols, (key value), which will be bound to each successive key/value
pair in `array'."
  (if result?
      (if (consp var)
          (destructuring-bind (key val) var
            `((lambda ()
                (let* (,val)
                  (for-in ((var ,key) ,array)
                    (setf ,val (aref ,array ,key))
                    ,@body)
                  (return ,result)))))
          `((lambda ()
              (for-in ((var ,var) ,array)
                ,@body)
              (return ,result))))
      (if (consp var)
          (destructuring-bind (key val) var
            `(progn
               (let* (,val)
                 (for-in ((var ,key) ,array)
                   (setf ,val (aref ,array ,key))
                   ,@body))))
          `(progn
             (for-in ((var ,var) ,array) ,@body)))))

(define-ps-special-form while (expecting test &rest body)
  (declare (ignore expecting))
  (list 'js-while (compile-parenscript-form test :expecting :expression)
                  (compile-parenscript-form `(progn ,@body))))

(defpsmacro dotimes ((var count &optional (result nil result?)) &rest body)
  `(do* ((,var 0 (1+ ,var)))
        ((>= ,var ,count) ,@(when result? (list result)))
     ,@body))

(defpsmacro dolist ((var array &optional (result nil result?)) &body body)
  (let ((idx (ps-gensym "_js_idx"))
        (arrvar (ps-gensym "_js_arrvar")))
    `(do* (,var
           (,arrvar ,array)
           (,idx 0 (1+ ,idx)))
          ((>= ,idx (slot-value ,arrvar 'length))
           ,@(when result? (list result)))
       (setq ,var (aref ,arrvar ,idx))
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc
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

(define-ps-special-form cc-if (expecting test &rest body)
  (declare (ignore expecting))
  (list 'cc-if test (mapcar #'compile-parenscript-form body)))

(define-ps-special-form regex (expecting regex)
  (declare (ignore expecting))
  (list 'js-regex (string regex)))

(define-ps-special-form lisp (expecting lisp-form)
  ;; (ps (foo (lisp bar))) is in effect equivalent to (ps* `(foo ,bar))
  ;; when called from inside of ps*, lisp-form has access only to the dynamic environment (like for eval)
  (declare (ignore expecting))
  (list 'js-escape lisp-form))
