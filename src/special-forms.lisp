(in-package "PARENSCRIPT")

(defmacro with-local-macro-environment ((var env) &body body)
  `(let* ((,var (make-macro-dictionary))
          (,env (cons ,var ,env)))
     ,@body))

(macrolet ((define-trivial-special-forms (&rest mappings)
             `(progn
                ,@(loop for (form-name js-primitive) on mappings by #'cddr
                       collect
                       `(define-ps-special-form ,form-name (&rest args)
                          (cons ',js-primitive
                                (mapcar #'compile-expression args)))))))
  (define-trivial-special-forms
    +          js:+
    -          js:-
    *          js:*
    /          js:/
    and        js:&&
    or         js:\|\|

    logand     js:&
    logior     js:\|
    logxor     js:^
    lognot     js:~
    ;; todo: ash for shifts

    throw      js:throw
    array      js:array
    aref       js:aref

    instanceof js:instanceof
    typeof     js:typeof
    new        js:new
    delete     js:delete
    in         js:in ;; maybe rename to slot-boundp?
    ))

(define-ps-special-form - (&rest args)
  (let ((args (mapcar #'compile-expression args)))
    (cons (if (cdr args) 'js:- 'js:negate) args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; n-ary equality

(defun fix-nary-comparison (operator objects)
  (let* ((tmp-var-forms (butlast (cdr objects)))
         (tmp-vars (loop repeat (length tmp-var-forms)
                         collect (ps-gensym "_cmp")))
         (all-comparisons (append (list (car objects))
                                  tmp-vars
                                  (last objects))))
    `(let ,(mapcar #'list tmp-vars tmp-var-forms)
       (and ,@(loop for x1 in all-comparisons
                    for x2 in (cdr all-comparisons)
                    collect (list operator x1 x2))))))

(macrolet ((define-nary-comparison-forms (&rest mappings)
             `(progn
                ,@(loop for (form js-primitive) on mappings by #'cddr collect
                       `(define-ps-special-form ,form (&rest objects)
                          (if (cddr objects)
                              (ps-compile
                               (fix-nary-comparison ',form objects))
                              (cons ',js-primitive
                                    (mapcar #'compile-expression objects))))))))
  (define-nary-comparison-forms
    <     js:<
    >     js:>
    <=    js:<=
    >=    js:>=
    equal js:===))

(define-ps-special-form /= (a b)
  ;; for n>2, /= is finding duplicates in an array of numbers (ie -
  ;; nontrivial runtime algorithm), so we restrict it to binary in PS
  `(js:!== ,(compile-expression a) ,(compile-expression b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; literals

(define-ps-special-form break (&optional label)
  `(js:break ,label))

(define-ps-special-form label (label &rest statements)
  `(js:label ,label
             ,(if (> (length statements) 1)
                  `(js:block ,@(mapcar #'ps-compile-statement statements))
                  (ps-compile-statement (car statements)))))

(define-ps-special-form quote (x)
  (flet ((quote% (expr) (when expr `',expr)))
    (compile-expression
     (typecase x
       (cons `(array ,@(mapcar #'quote% x)))
       (null '(array))
       (keyword x)
       (symbol (symbol-to-js-string x))
       (number x)
       (string x)
       (vector `(array ,@(loop :for el :across x :collect (quote% el))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; statements

(defun ps-statement? (exp)
  (and (consp exp)
       (member (car exp)
               '(throw
                 for
                 for-in
                 while))))

(defun implicit-progn-form? (form)
  (member (car form) '(with progn label let flet labels macrolet symbol-macrolet)))

(define-ps-special-form return (&optional value force-conditional?)
  (let ((value (ps-macroexpand value)))
    (if (ps-statement? value)
        (compile-statement value)
        (if (consp value)
            (if (implicit-progn-form? value)
                (ps-compile (append (butlast value)
                                    `((return ,@(last value)
                                              ,force-conditional?))))
                (case (car value)
                  (return
                    (ps-compile value))
                  (switch
                    (ps-compile
                     `(switch ,(second value)
                       ,@(loop for (cvalue . cbody) in (cddr value)
                            for remaining on (cddr value) collect
                              (let ((last-n
                                     (cond ((or (eq 'default cvalue)
                                                (not (cdr remaining)))
                                            1)
                                           ((eq 'break
                                                (car (last cbody)))
                                            2))))
                                (if last-n
                                    `(,cvalue
                                      ,@(butlast cbody last-n)
                                      (return
                                        ,(car (last cbody last-n))
                                        t))
                                    (cons cvalue cbody)))))))
                  (try
                   (ps-compile
                    `(try (return ,(second value) t)
                          ,@(let ((catch (cdr (assoc :catch (cdr value))))
                                  (finally (assoc :finally (cdr value))))
                                 (list (when catch
                                         `(:catch ,(car catch)
                                            ,@(butlast (cdr catch))
                                            (return ,@(last (cdr catch)) t)))
                                       finally)))))
                  (if
                   (ps-compile `(if ,(second value)
                                    (return ,(third value) ,force-conditional?)
                                    ,@(acond ((fourth value)
                                              `((return ,it
                                                        ,force-conditional?)))
                                             (force-conditional?
                                              '((return nil)))))))
                  (cond
                    (ps-compile `(cond
                                   ,@(loop for clause in (cdr value) collect
                                          `(,@(butlast clause)
                                              (return ,@(last clause)
                                                      ,force-conditional?))))))
                  (otherwise
                   `(js:return ,(compile-expression value)))))
            `(js:return ,(compile-expression value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operators

(define-ps-special-form incf (x &optional (delta 1))
  (let ((delta (ps-macroexpand delta)))
    (if (eql delta 1)
        `(js:++ ,(compile-expression x))
        `(js:+= ,(compile-expression x) ,(compile-expression delta)))))

(define-ps-special-form decf (x &optional (delta 1))
  (let ((delta (ps-macroexpand delta)))
    (if (eql delta 1)
        `(js:-- ,(compile-expression x))
        `(js:-= ,(compile-expression x) ,(compile-expression delta)))))

(let ((inverses (mapcan (lambda (x)
                          (list x (reverse x)))
                        '((js:=== js:!==)
                          (js:== js:!=)
                          (js:< js:>=)
                          (js:> js:<=)))))
  (define-ps-special-form not (x)
    (let ((form (compile-expression x)))
      (acond ((and (listp form) (eq (car form) 'js:!))
              (second form))
             ((and (listp form) (cadr (assoc (car form) inverses)))
              `(,it ,@(cdr form)))
             (t `(js:! ,form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; progn

(defun flatten-blocks (body)
  (when body
    (if (and (listp (car body))
             (eq 'js:block (caar body)))
        (append (cdr (car body)) (flatten-blocks (cdr body)))
        (cons (car body) (flatten-blocks (cdr body))))))

(define-ps-special-form progn (&rest body)
  (let ((body (mapcar #'ps-macroexpand body)))
    (if (and compile-expression? (= 1 (length body)))
        (compile-expression (car body))
        `(,(if compile-expression? 'js:|,| 'js:block)
           ,@(let* ((block (flatten-blocks
                            (remove nil (mapcar #'ps-compile body))))
                    (last (last block)))
               (append (remove-if #'constantp (butlast block))
                       (if (and (eq *ps-compilation-level* :toplevel)
                                (not (car last)))
                           nil
                           (last block))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conditionals

(define-ps-special-form cond (&rest clauses)
  (if compile-expression?
      (make-cond-clauses-into-nested-ifs clauses)
      `(js:if ,(compile-expression (caar clauses))
              ,(compile-statement `(progn ,@(cdar clauses)))
              ,@(loop for (test . body) in (cdr clauses) appending
                     (if (eq t test)
                         `(:else ,(compile-statement `(progn ,@body)))
                         `(:else-if ,(compile-expression test)
                                    ,(compile-statement `(progn ,@body))))))))

(defun make-cond-clauses-into-nested-ifs (clauses)
  (if clauses
      (destructuring-bind (test &rest body)
          (car clauses)
        (if (eq t test)
            (compile-expression `(progn ,@body))
            `(js:? ,(compile-expression test)
                   ,(compile-expression `(progn ,@body))
                   ,(make-cond-clauses-into-nested-ifs (cdr clauses)))))
      (compile-expression nil)))

(define-ps-special-form if (test then &optional else)
  (if compile-expression?
      `(js:? ,(compile-expression test)
             ,(compile-expression then)
             ,(compile-expression else))
      `(js:if ,(compile-expression test)
              ,(compile-statement `(progn ,then))
              ,@(when else `(:else ,(compile-statement `(progn ,else)))))))

(define-ps-special-form switch (test-expr &rest clauses)
  `(js:switch ,(compile-expression test-expr)
     ,(loop for (val . body) in clauses collect
           (cons (if (eq val 'default)
                     'js:default
                     (compile-expression val))
                 (mapcan (lambda (x)
                           (let ((exp (compile-statement x)))
                             (if (and (listp exp) (eq 'js:block (car exp)))
                                 (cdr exp)
                                 (list exp))))
                         body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function definition

(defun add-implicit-return (fbody)
  (let ((last-thing (car (last fbody))))
    (if (ps-statement? last-thing)
        fbody
        (append (butlast fbody)
                `((return ,last-thing))))))

(defmacro with-declaration-effects (body-var &body body)
  `(let* ((local-specials (when (and (listp (car ,body-var))
                                     (eq (caar ,body-var) 'declare))
                            (cdr (find 'special (cdar ,body-var) :key #'car))))
          (,body-var (if local-specials
                         (cdr ,body-var)
                         ,body-var))
          (*ps-special-variables*
           (append local-specials *ps-special-variables*)))
     ,@body))

(defun compile-function-definition (args body)
  (with-declaration-effects body
    (list args
          (let* ((*enclosing-lexical-block-declarations* ())
                 (*ps-enclosing-lexicals*
                  (append args *ps-enclosing-lexicals*))
                 (body
                  (compile-statement `(progn
                                        ,@(add-implicit-return body))))
                 (var-decls
                  (compile-statement
                   `(progn
                      ,@(mapcar (lambda (var)
                                  `(var ,var))
                                *enclosing-lexical-block-declarations*)))))
            `(js:block ,@(cdr var-decls) ,@(cdr body))))))

(define-ps-special-form %js-lambda (args &rest body)
  `(js:lambda ,@(compile-function-definition args body)))

(define-ps-special-form %js-defun (name args &rest body)
  `(js:defun ,name ,@(compile-function-definition args body)))

(defun parse-function-body (body)
  (let* ((docstring (when (stringp (first body))
                      (first body)))
         (body-forms (if docstring (rest body) body)))
    (values body-forms docstring)))

(defun parse-key-spec (key-spec)
  "parses an &key parameter.  Returns 5 values:
var, init-form,  keyword-name, supplied-p-var, init-form-supplied-p.

Syntax of key spec:
[&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}*
"
  (let* ((var (cond ((symbolp key-spec) key-spec)
                    ((and (listp key-spec) (symbolp (first key-spec))) (first key-spec))
                    ((and (listp key-spec) (listp (first key-spec)))   (second (first key-spec)))))
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

(defpsmacro defaultf (name value suppl)
  `(progn
     ,@(when suppl `((var ,suppl t)))
     (when (eql ,name undefined)
       (setf ,name ,value ,@(when suppl (list suppl nil))))))

(defun parse-extended-function (lambda-list body)
  "Returns two values: the effective arguments and body for a function with
the given lambda-list and body."

  ;; The lambda list is transformed as follows, since a javascript
  ;; lambda list is just a list of variable names, and you have access
  ;; to the arguments variable inside the function:

  ;; * standard variables are the mapped directly into the js-lambda
  ;;   list

  ;; * optional variables' variable names are mapped directly into the
  ;;   lambda list, and for each optional variable with name v,
  ;;   default value d, and supplied-p parameter s, a form is produced
  ;;   (defaultf v d s)

  ;; * keyword variables are not included in the js-lambda list, but
  ;;   instead are obtained from the magic js ARGUMENTS
  ;;   pseudo-array. Code assigning values to keyword vars is
  ;;   prepended to the body of the function. Defaults and supplied-p
  ;;   are handled using the same mechanism as with optional vars.
  (multiple-value-bind (requireds optionals rest? rest keys? keys allow? aux?
                                  aux more? more-context more-count key-object)
      (parse-lambda-list lambda-list)
    (declare (ignore allow? aux? aux more? more-context more-count key-object))
    (let* ( ;; optionals are of form (var default-value)
           (effective-args
            (remove-if #'null
                       (append requireds
                               (mapcar #'parse-optional-spec optionals))))
           (opt-forms
            (mapcar (lambda (opt-spec)
                      (multiple-value-bind (var val suppl)
                          (parse-optional-spec opt-spec)
                        `(defaultf ,var ,val ,suppl)))
                    optionals))
           (key-forms
            (when keys?
              (if (< *js-target-version* 1.6)
                  (with-ps-gensyms (n)
                    (let ((decls nil)
                          (assigns nil)
                          (defaults nil))
                      (mapc
                       (lambda (k)
                         (multiple-value-bind (var init-form
                                                   keyword-str suppl)
                             (parse-key-spec k)
                           (push `(var ,var)
                                 decls)
                           (push `(,keyword-str (setf ,var (aref arguments (1+ ,n))))
                                 assigns)
                           (push (list 'defaultf var init-form suppl)
                                 defaults)))
                       (reverse keys))
                      `(,@decls
                        (loop for ,n from ,(length requireds)
                           below (length arguments) by 2 do
                           (case (aref arguments ,n) ,@assigns))
                        ,@defaults)))
                  (mapcar
                   (lambda (k)
                     (multiple-value-bind (var init-form keyword-str)
                         (parse-key-spec k)
                       (with-ps-gensyms (x)
                         `(let ((,x ((@ *Array prototype index-of call)
                                     arguments ,keyword-str
                                     ,(length requireds))))
                            (var ,var (if (= -1 ,x)
                                          ,init-form
                                          (aref arguments (1+ ,x))))))))
                   keys))))
           (rest-form
            (when rest?
              (with-ps-gensyms (i)
                `(progn (var ,rest (array))
                        (dotimes (,i (- (getprop arguments 'length)
                                        ,(length effective-args)))
                          (setf (aref ,rest
                                      ,i)
                                (aref arguments
                                      (+ ,i ,(length effective-args)))))))))
           (body-paren-forms (parse-function-body body))
           (effective-body (append opt-forms
                                   key-forms
                                   (awhen rest-form (list it))
                                   body-paren-forms)))
      (values effective-args effective-body))))

(defun maybe-rename-local-function (fun-name)
  (aif (getf *ps-local-function-names* fun-name)
       it
       fun-name))

(defun collect-function-names (fn-defs)
  (loop for (fn-name) in fn-defs
     collect fn-name
     collect (if (or (member fn-name *ps-enclosing-lexicals*)
                     (lookup-macro-def fn-name *ps-symbol-macro-env*))
                 (ps-gensym fn-name)
                 fn-name)))

(define-ps-special-form flet (fn-defs &rest body)
  (let* ((fn-renames (collect-function-names fn-defs))
         (fn-defs (loop for (fn-name . def) in fn-defs collect
                       (ps-compile `(var ,(getf fn-renames fn-name)
                                         (lambda ,@def)))))
         (*ps-enclosing-lexicals*
          (append fn-renames *ps-enclosing-lexicals*))
         (*ps-local-function-names*
          (append fn-renames *ps-local-function-names*)))
    `(,(if compile-expression? 'js:|,| 'js:block)
       ,@fn-defs
       ,@(flatten-blocks (mapcar #'ps-compile body)))))

(define-ps-special-form labels (fn-defs &rest body)
  (let* ((fn-renames (collect-function-names fn-defs))
         (*ps-local-function-names*
          (append fn-renames *ps-local-function-names*))
         (*ps-enclosing-lexicals*
          (append fn-renames *ps-enclosing-lexicals*)))
    (ps-compile
     `(progn ,@(loop for (fn-name . def) in fn-defs collect
                    `(var ,(getf *ps-local-function-names* fn-name)
                          (lambda ,@def)))
             ,@body))))

(define-ps-special-form function (fn-name)
  (ps-compile (maybe-rename-local-function fn-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macros

(define-ps-special-form macrolet (macros &body body)
  (with-local-macro-environment (local-macro-dict *ps-macro-env*)
    (dolist (macro macros)
      (destructuring-bind (name arglist &body body)
          macro
        (setf (gethash name local-macro-dict)
              (eval (make-ps-macro-function arglist body)))))
    (ps-compile `(progn ,@body))))

(define-ps-special-form symbol-macrolet (symbol-macros &body body)
  (with-local-macro-environment (local-macro-dict *ps-symbol-macro-env*)
    (let (local-var-bindings)
      (dolist (macro symbol-macros)
        (destructuring-bind (name expansion)
            macro
          (setf (gethash name local-macro-dict) (lambda (x)
                                                  (declare (ignore x))
                                                  expansion))
          (push name local-var-bindings)))
      (let ((*ps-enclosing-lexicals*
             (append local-var-bindings
                     *ps-enclosing-lexicals*)))
        (ps-compile `(progn ,@body))))))

(define-ps-special-form defmacro (name args &body body)
  (eval `(defpsmacro ,name ,args ,@body))
  nil)

(define-ps-special-form define-symbol-macro (name expansion)
  (eval `(define-ps-symbol-macro ,name ,expansion))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; objects

(define-ps-symbol-macro {} (create))

(define-ps-special-form create (&rest arrows)
  `(js:object
    ,@(loop for (key val-expr) on arrows by #'cddr collecting
           (progn
             (assert (or (stringp key) (numberp key) (symbolp key))
                     ()
                     "Slot key ~s is not one of symbol, string or number."
                     key)
             (cons (aif (and (symbolp key) (ps-reserved-symbol? key)) it key)
                   (compile-expression val-expr))))))

(define-ps-special-form %js-getprop (obj slot)
  (let ((expanded-slot (ps-macroexpand slot))
        (obj (compile-expression obj)))
    (if (and (listp expanded-slot)
             (eq 'quote (car expanded-slot)))
        (aif (or (ps-reserved-symbol? (second expanded-slot))
                 (and (keywordp (second expanded-slot)) (second expanded-slot)))
             `(js:aref ,obj ,it)
             `(js:getprop ,obj ,(second expanded-slot)))
        `(js:aref ,obj ,(compile-expression slot)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assignment and binding

(defun assignment-op (op)
  (getf '(js:+   js:+=
          js:~   js:~=
          js:&   js:&=
          js:\|  js:\|=
          js:-   js:-=
          js:*   js:*=
          js:%   js:%=
          js:>>  js:>>=
          js:^   js:^=
          js:<<  js:<<=
          js:>>> js:>>>=
          js:/   js:/=)
        op))

(define-ps-special-form ps-assign (lhs rhs)
  (let ((lhs (compile-expression lhs))
        (rhs (compile-expression rhs)))
    (aif (and (listp rhs)
              (= 3 (length rhs))
              (equal lhs (second rhs))
              (assignment-op (first rhs)))
         (list it lhs (if (fourth rhs)
                          (cons (first rhs) (cddr rhs))
                          (third rhs)))
         (list 'js:= lhs rhs))))

(define-ps-special-form var (name &optional
                                  (value (values) value-provided?)
                                  documentation)
  (declare (ignore documentation))
  (let ((name (ps-macroexpand name)))
    (if compile-expression?
        (progn (push name *enclosing-lexical-block-declarations*)
               (when value-provided?
                 (compile-expression `(setf ,name ,value))))
        `(js:var ,name
                 ,@(when value-provided?
                         (list (compile-expression value)))))))

(define-ps-special-form let (bindings &body body)
  (with-declaration-effects body
    (let* ((lexical-bindings-introduced-here ())
           (normalized-bindings
            (mapcar (lambda (x)
                      (if (symbolp x)
                          (list x nil)
                          (list (car x) (ps-macroexpand (cadr x)))))
                    bindings))
           (free-variables-in-binding-value-expressions
            (mapcan (lambda (x)
                      (flatten (cadr x)))
                    normalized-bindings)))
      (flet ((maybe-rename-lexical-var (x)
               (if (or (member x *ps-enclosing-lexicals*)
                       (lookup-macro-def x *ps-symbol-macro-env*)
                       (member x free-variables-in-binding-value-expressions))
                   (ps-gensym x)
                   (progn (push x lexical-bindings-introduced-here) nil)))
             (rename (x) (first x))
             (var (x) (second x))
             (val (x) (third x)))
        (let* ((lexical-bindings
                (loop for x in normalized-bindings
                   unless (ps-special-variable-p (car x))
                   collect (cons (maybe-rename-lexical-var (car x)) x)))
               (dynamic-bindings
                (loop for x in normalized-bindings
                   when (ps-special-variable-p (car x))
                   collect (cons (ps-gensym (format nil "~A_~A"
                                                    (car x) 'tmp-stack))
                                 x)))
               (renamed-body `(symbol-macrolet ,(loop for x in lexical-bindings
                                                   when (rename x) collect
                                                   `(,(var x) ,(rename x)))
                                ,@body))
               (*ps-enclosing-lexicals*
                (append lexical-bindings-introduced-here
                        *ps-enclosing-lexicals*)))
          (ps-compile
           `(progn
              ,@(mapcar (lambda (x)
                          `(var ,(or (rename x)
                                     (var x))
                                ,(val x)))
                        lexical-bindings)
              ,(if dynamic-bindings
                   `(progn ,@(mapcar (lambda (x)
                                       `(var ,(rename x)))
                                     dynamic-bindings)
                           (try (progn
                                  (setf ,@(loop for x in dynamic-bindings append
                                               `(,(rename x) ,(var x)
                                                  ,(var x) ,(val x))))
                                  ,renamed-body)
                                (:finally
                                 (setf ,@(mapcan (lambda (x)
                                                   `(,(var x) ,(rename x)))
                                                 dynamic-bindings)))))
                   renamed-body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iteration

(defun make-for-vars/inits (init-forms)
  (mapcar (lambda (x)
            (cons (ps-macroexpand (if (atom x) x (first x)))
                  (compile-expression (if (atom x) nil (second x)))))
          init-forms))

(define-ps-special-form for (init-forms cond-forms step-forms &body body)
  `(js:for ,(make-for-vars/inits init-forms)
     ,(mapcar #'compile-expression cond-forms)
     ,(mapcar #'compile-expression step-forms)
     ,(compile-statement `(progn ,@body))))

(define-ps-special-form continue (&optional label)
  `(js:continue ,label))

(define-ps-special-form for-in ((var object) &rest body)
  `(js:for-in ,(compile-expression var)
              ,(compile-expression object)
              ,(compile-statement `(progn ,@body))))

(define-ps-special-form while (test &rest body)
  `(js:while ,(compile-expression test)
     ,(compile-statement `(progn ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc

(define-ps-special-form with (expression &rest body)
  `(js:with ,(compile-expression expression)
     ,(compile-statement `(progn ,@body))))

(define-ps-special-form try (form &rest clauses)
  (let ((catch (cdr (assoc :catch clauses)))
        (finally (cdr (assoc :finally clauses))))
    (assert (not (cdar catch)) nil "Sorry, currently only simple catch forms are supported.")
    (assert (or catch finally) ()
            "Try form should have either a catch or a finally clause or both.")
    `(js:try ,(compile-statement `(progn ,form))
             :catch ,(when catch (list (caar catch)
                                       (compile-statement `(progn ,@(cdr catch)))))
             :finally ,(when finally (compile-statement `(progn ,@finally))))))

(define-ps-special-form regex (regex)
  `(js:regex ,(string regex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalutation

(define-ps-special-form lisp (lisp-form)
  ;; (ps (foo (lisp bar))) is like (ps* `(foo ,bar))
  ;; When called from inside of ps*, lisp-form has access to the
  ;; dynamic environment only, analogoues to eval.
  `(js:escape
    (with-output-to-string (*psw-stream*)
      (let ((compile-expression? ,compile-expression?))
        (parenscript-print (ps-compile ,lisp-form) t)))))

(define-ps-special-form eval-when (situation-list &body body)
  "The body is evaluated only during the given situations. The
accepted situations are :load-toplevel, :compile-toplevel,
and :execute. The code in BODY is assumed to be Common-Lisp code
in :compile-toplevel and :load-toplevel sitations, and Parenscript
code in :execute."
  (when (and (member :compile-toplevel situation-list)
	     (member *ps-compilation-level* '(:toplevel :inside-toplevel-form)))
    (eval `(progn ,@body)))
  (if (member :execute situation-list)
      (ps-compile `(progn ,@body))
      (ps-compile `(progn))))
