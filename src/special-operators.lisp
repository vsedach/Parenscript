(in-package #:parenscript)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; arithmetic and logic

(macrolet ((define-trivial-special-ops (&rest mappings)
             `(progn
                ,@(loop for (form-name js-primitive) on mappings by #'cddr
                       collect
                       `(define-expression-operator ,form-name (&rest args)
                          (cons ',js-primitive
                                (mapcar #'compile-expression args)))))))
  (define-trivial-special-ops
    +          js:+
    -          js:-
    *          js:*
    /          js:/
    rem        js:%
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
    break      js:break
    funcall    js:funcall
    ))

(define-expression-operator - (&rest args)
  (let ((args (mapcar #'compile-expression args)))
    (cons (if (cdr args) 'js:- 'js:negate) args)))

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
                       `(define-expression-operator ,form (&rest objects)
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
    eql   js:===
    equal js:==))

(define-expression-operator /= (a b)
  ;; for n>2, /= is finding duplicates in an array of numbers (ie -
  ;; nontrivial runtime algorithm), so we restrict it to binary in PS
  `(js:!== ,(compile-expression a) ,(compile-expression b)))

(define-expression-operator incf (x &optional (delta 1))
  (let ((delta (ps-macroexpand delta)))
    (if (eql delta 1)
        `(js:++ ,(compile-expression x))
        `(js:+= ,(compile-expression x) ,(compile-expression delta)))))

(define-expression-operator decf (x &optional (delta 1))
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
  (define-expression-operator not (x)
    (let ((form (compile-expression x)))
      (acond ((and (listp form) (eq (car form) 'js:!))
              (second form))
             ((and (listp form) (cadr (assoc (car form) inverses)))
              `(,it ,@(cdr form)))
             (t `(js:! ,form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; blocks

(defun compile-progn (body)
  (labels ((flatten-blocks (body)
             (when body
               (if (and (listp (car body)) (eq 'js:block (caar body)))
                   (append (cdr (car body)) (flatten-blocks (cdr body)))
                   (cons (car body) (flatten-blocks (cdr body)))))))
    (let ((block (flatten-blocks (remove nil (mapcar #'ps-compile body)))))
      (append (remove-if #'constantp (butlast block))
              (unless (and (eq *compilation-level* :toplevel)
                           (not (car (last block))))
                (last block))))))

(define-expression-operator progn (&rest body)
  (if (cdr body)
      `(js:|,| ,@(compile-progn body))
      (compile-expression (car body))))

(define-statement-operator progn (&rest body)
  `(js:block ,@(compile-progn body)))

(define-statement-operator label (label &rest body) ;; does label need to do symbol-macro expansion?
  `(js:label ,label ,(compile-statement `(progn ,@body))))

(define-statement-operator continue (&optional label)
  `(js:continue ,label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conditionals

(define-expression-operator if (test then &optional else)
   `(js:? ,(compile-expression test) ,(compile-expression then) ,(compile-expression else)))

(define-statement-operator if (test then &optional else)
  `(js:if ,(compile-expression test)
          ,(compile-statement `(progn ,then))
          ,@(when else `(:else ,(compile-statement `(progn ,else))))))

(define-expression-operator cond (&rest clauses)
  (compile-expression
   (when clauses
     (destructuring-bind (test &rest body) (car clauses)
       (if (eq t test)
           `(progn ,@body)
           `(if ,test
                (progn ,@body)
                (cond ,@(cdr clauses))))))))

(define-statement-operator cond (&rest clauses)
  `(js:if ,(compile-expression (caar clauses))
          ,(compile-statement `(progn ,@(cdar clauses)))
          ,@(loop for (test . body) in (cdr clauses) appending
                 (if (eq t test)
                     `(:else ,(compile-statement `(progn ,@body)))
                     `(:else-if ,(compile-expression test)
                                ,(compile-statement `(progn ,@body)))))))

(define-statement-operator switch (test-expr &rest clauses)
  `(js:switch ,(compile-expression test-expr)
     ,@(loop for (val . body) in clauses collect
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
;;; function

(defun nesting-depth (form)
  (if (consp form)
      (max (1+ (nesting-depth (car form))) (nesting-depth (cdr form)))
      0))

(define-statement-operator return-from (tag &optional form)
  (let ((form (ps-macroexpand form)))
    (if (listp form)
        (block expressionize
          (ps-compile
           (case (car form)
             (progn
               `(progn ,@(butlast (cdr form)) (return-from ,tag ,(car (last (cdr form))))))
             (switch
               `(switch ,(second form)
                  ,@(loop for (cvalue . cbody) in (cddr form)
                          for remaining on (cddr form) collect
                         (let ((last-n (cond ((or (eq 'default cvalue) (not (cdr remaining)))
                                              1)
                                             ((eq 'break (car (last cbody)))
                                              2))))
                           (if last-n
                               (let ((result-form (car (last cbody last-n))))
                                 `(,cvalue
                                   ,@(butlast cbody last-n)
                                   (return-from ,tag ,result-form)
                                   ,@(when (and (= last-n 2) (member 'if (flatten result-form))) '(break))))
                               (cons cvalue cbody))))))
             (try
              `(try (return-from ,tag ,(second form))
                    ,@(let ((catch (cdr (assoc :catch (cdr form))))
                            (finally (assoc :finally (cdr form))))
                           (list (when catch
                                   `(:catch ,(car catch)
                                      ,@(butlast (cdr catch))
                                      (return-from ,tag ,(car (last (cdr catch))))))
                                 finally))))
             (cond
               `(cond ,@(loop for clause in (cdr form) collect
                             `(,@(butlast clause)
                                 (return-from ,tag ,(car (last clause)))))))
             ((with label let flet labels macrolet symbol-macrolet) ;; implicit progn forms
              `(,(first form) ,(second form)
                 ,@(butlast (cddr form))
                 (return-from ,tag ,(car (last (cddr form))))))
             ((continue break throw) ;; non-local exit
              form)
             (return-from ;; this will go away someday
               (unless tag
                 (warn 'simple-style-warning
                       :format-control "Trying to RETURN a RETURN without a block tag specified. Perhaps you're still returning values from functions by hand? Parenscript now implements implicit return, update your code! Things like (lambda () (return x)) are not valid Common Lisp and may not be supported in future versions of Parenscript."))
               form)
             (if
              (aif (and (<= (nesting-depth form) 3) (handler-case (compile-expression form) (compile-expression-error () nil)))
                  (return-from expressionize `(js:return ,it))
                  `(if ,(second form)
                       (return-from ,tag ,(third form))
                       ,@(when (fourth form) `((return-from ,tag ,(fourth form)))))))
             (otherwise
              (if (gethash (car form) *special-statement-operators*)
                  form ;; by now only special forms that return nil should be left, so this is ok
                  (return-from expressionize `(js:return ,(compile-expression form))))))))
        `(js:return ,(compile-expression form)))))

(defmacro with-declaration-effects (body-var &body body)
  `(let* ((local-specials (when (and (listp (car ,body-var))
                                     (eq (caar ,body-var) 'declare))
                            (cdr (find 'special (cdar ,body-var) :key #'car))))
          (,body-var (if local-specials
                         (cdr ,body-var)
                         ,body-var))
          (*special-variables* (append local-specials *special-variables*)))
     ,@body))

(defun compile-function-definition (args body)
  (with-declaration-effects body
    (let* ((*enclosing-lexical-block-declarations* ())
           (*enclosing-lexicals*                   (append args *enclosing-lexicals*))
           (body                                   (let ((in-loop-scope?                 nil)
                                                         (*loop-scope-lexicals*          ())
                                                         (*loop-scope-lexicals-captured* ()))
                                                     (compile-statement `(return-from %function-body (progn ,@body)))))
           (var-decls                              (compile-statement
                                                    `(progn ,@(mapcar (lambda (var) `(var ,var))
                                                                      (remove-duplicates *enclosing-lexical-block-declarations*))))))
      (when in-loop-scope? ;; this is probably broken when it comes to let-renaming
        (setf *loop-scope-lexicals-captured* (append (intersection (flatten body) *loop-scope-lexicals*)
                                                     *loop-scope-lexicals-captured*)))
      `(js:block ,@(cdr var-decls) ,@(cdr body)))))

(define-expression-operator %js-lambda (args &rest body)
  `(js:lambda ,args ,(compile-function-definition args body)))

(define-statement-operator %js-defun (name args &rest body)
  (let ((docstring (and (cdr body) (stringp (car body)) (car body))))
    `(js:defun ,name ,args ,docstring
               ,(compile-function-definition args (if docstring (cdr body) body)))))

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

(defun parse-extended-function (lambda-list body)
  ;; The lambda list is transformed as follows:

  ;; * standard and optional variables are the mapped directly into
  ;;   the js-lambda list

  ;; * keyword variables are not included in the js-lambda list, but
  ;;   instead are obtained from the magic js ARGUMENTS
  ;;   pseudo-array. Code assigning values to keyword vars is
  ;;   prepended to the body of the function.
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
                      (multiple-value-bind (name value suppl)
                          (parse-optional-spec opt-spec)
                        (if suppl
                            `(progn
                               (var ,suppl (not (eql ,name undefined)))
                               ,@(when value
                                   `((when (not ,suppl) (setf ,name ,value)))))
                            (when value
                              `(when (eql ,name undefined)
                                 (setf ,name ,value))))))
                    optionals))
           (key-forms
            (when keys?
              (with-ps-gensyms (n)
                (let ((decls ())
                      (assigns ()))
                  (mapc
                   (lambda (k)
                     (multiple-value-bind (var init-form keyword-str suppl)
                         (parse-key-spec k)
                       (push `(var ,var ,init-form) decls)
                       (when suppl (push `(var ,suppl nil) decls))
                       (push `(,keyword-str
                               (setf ,var (aref arguments (1+ ,n))
                                     ,@(when suppl `(,suppl t))))
                             assigns)))
                   (reverse keys))
                  `(,@decls
                    (loop for ,n from ,(length requireds)
                       below (length arguments) by 2 do
                         (case (aref arguments ,n) ,@assigns)))))))
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
           (docstring (when (stringp (first body)) (first body)))
           (body-paren-forms (if docstring (rest body) body))
           (effective-body (append (when docstring (list docstring))
                                   opt-forms
                                   key-forms
                                   (awhen rest-form (list it))
                                   body-paren-forms)))
      (values effective-args effective-body))))

(defun maybe-rename-local-function (fun-name)
  (aif (getf *local-function-names* fun-name)
       it
       fun-name))

(defun collect-function-names (fn-defs)
  (loop for (fn-name) in fn-defs
        collect fn-name
        collect (if (or (member fn-name *enclosing-lexicals*) (lookup-macro-def fn-name *symbol-macro-env*))
                    (ps-gensym fn-name)
                    fn-name)))

(define-expression-operator flet (fn-defs &rest body)
  (let* ((fn-renames                 (collect-function-names fn-defs))
         ;; the function definitions need to be compiled with previous lexical bindings
         (fn-defs                    (loop for (fn-name . def) in fn-defs collect
                                           (ps-compile `(var ,(getf fn-renames fn-name) (lambda ,@def)))))
         ;; the flet body needs to be compiled with the extended lexical environment
         (*enclosing-lexicals*       (append fn-renames *enclosing-lexicals*))
         (*loop-scope-lexicals*      (when in-loop-scope? (append fn-renames *loop-scope-lexicals*)))
         (*local-function-names*  (append fn-renames *local-function-names*)))
    `(,(if compile-expression? 'js:|,| 'js:block)
       ,@fn-defs
       ,@(compile-progn body))))

(define-expression-operator labels (fn-defs &rest body)
  (let* ((fn-renames                 (collect-function-names fn-defs))
         (*local-function-names*  (append fn-renames *local-function-names*))
         (*enclosing-lexicals*       (append fn-renames *enclosing-lexicals*))
         (*loop-scope-lexicals*      (when in-loop-scope? (append fn-renames *loop-scope-lexicals*))))
    (ps-compile
     `(progn ,@(loop for (fn-name . def) in fn-defs collect
                    `(var ,(getf *local-function-names* fn-name) (lambda ,@def)))
             ,@body))))

(define-expression-operator function (fn-name)
  (ps-compile (maybe-rename-local-function fn-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macros

(defmacro with-local-macro-environment ((var env) &body body)
  `(let* ((,var (make-macro-dictionary))
          (,env (cons ,var ,env)))
     ,@body))

(define-expression-operator macrolet (macros &body body)
  (with-local-macro-environment (local-macro-dict *macro-env*)
    (dolist (macro macros)
      (destructuring-bind (name arglist &body body)
          macro
        (setf (gethash name local-macro-dict)
              (eval (make-ps-macro-function arglist body)))))
    (ps-compile `(progn ,@body))))

(define-expression-operator symbol-macrolet (symbol-macros &body body)
  (with-local-macro-environment (local-macro-dict *symbol-macro-env*)
    (let (local-var-bindings)
      (dolist (macro symbol-macros)
        (destructuring-bind (name expansion) macro
          (setf (gethash name local-macro-dict) (lambda (x) (declare (ignore x)) expansion))
          (push name local-var-bindings)))
      (let ((*enclosing-lexicals* (append local-var-bindings *enclosing-lexicals*)))
        (ps-compile `(progn ,@body))))))

(define-expression-operator defmacro (name args &body body)
  (eval `(defpsmacro ,name ,args ,@body))
  nil)

(define-expression-operator define-symbol-macro (name expansion)
  (eval `(define-ps-symbol-macro ,name ,expansion))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; objects

(define-expression-operator create (&rest arrows)
  `(js:object
    ,@(loop for (key val-expr) on arrows by #'cddr collecting
           (progn
             (assert (or (stringp key) (numberp key) (symbolp key))
                     ()
                     "Slot key ~s is not one of symbol, string or number."
                     key)
             (cons (aif (and (symbolp key) (reserved-symbol? key)) it key)
                   (compile-expression val-expr))))))

(define-expression-operator %js-getprop (obj slot)
  (let ((expanded-slot (ps-macroexpand slot))
        (obj (compile-expression obj)))
    (if (and (listp expanded-slot)
             (eq 'quote (car expanded-slot)))
        (aif (or (reserved-symbol? (second expanded-slot))
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

(define-expression-operator ps-assign (lhs rhs)
  (let ((rhs (ps-macroexpand rhs)))
    (if (and (listp rhs) (eq (car rhs) 'progn))
        (ps-compile `(progn ,@(butlast (cdr rhs)) (ps-assign ,lhs ,(car (last (cdr rhs))))))
        (let ((lhs (compile-expression lhs))
              (rhs (compile-expression rhs)))
          (aif (and (listp rhs)
                    (= 3 (length rhs))
                    (equal lhs (second rhs))
                    (assignment-op (first rhs)))
               (list it lhs (if (fourth rhs)
                                (cons (first rhs) (cddr rhs))
                                (third rhs)))
               (list 'js:= lhs rhs))))))

(define-expression-operator var (name &optional (value (values) value?) docstr)
  (declare (ignore docstr))
  (push name *enclosing-lexical-block-declarations*)
  (when value? (compile-expression `(setf ,name ,value))))

(define-statement-operator var (name &optional (value (values) value?) docstr)
  `(js:var ,(ps-macroexpand name) ,@(when value? (list (compile-expression value) docstr))))

(define-expression-operator let (bindings &body body)
  (with-declaration-effects body
    (let* ((lexical-bindings-introduced-here             ())
           (normalized-bindings                          (mapcar (lambda (x)
                                                                   (if (symbolp x)
                                                                       (list x nil)
                                                                       (list (car x) (ps-macroexpand (cadr x)))))
                                                                 bindings))
           (free-variables-in-binding-value-expressions  (mapcan (lambda (x) (flatten (cadr x)))
                                                                 normalized-bindings)))
      (flet ((maybe-rename-lexical-var (x)
               (if (or (member x *enclosing-lexicals*)
                       (lookup-macro-def x *symbol-macro-env*)
                       (member x free-variables-in-binding-value-expressions))
                   (ps-gensym x)
                   (progn (push x lexical-bindings-introduced-here) nil)))
             (rename (x) (first x))
             (var (x) (second x))
             (val (x) (third x)))
        (let* ((lexical-bindings      (loop for x in normalized-bindings
                                            unless (special-variable? (car x))
                                            collect (cons (maybe-rename-lexical-var (car x)) x)))
               (dynamic-bindings      (loop for x in normalized-bindings
                                            when (special-variable? (car x))
                                            collect (cons (ps-gensym (format nil "~A_~A" (car x) 'tmp-stack)) x)))
               (renamed-body          `(symbol-macrolet ,(loop for x in lexical-bindings
                                                               when (rename x) collect
                                                               `(,(var x) ,(rename x)))
                                          ,@body))
               (*enclosing-lexicals*  (append lexical-bindings-introduced-here *enclosing-lexicals*))
               (*loop-scope-lexicals* (when in-loop-scope? (append lexical-bindings-introduced-here *loop-scope-lexicals*))))
          (ps-compile
           `(progn
              ,@(mapcar (lambda (x) `(var ,(or (rename x) (var x)) ,(val x)))
                        lexical-bindings)
              ,(if dynamic-bindings
                   `(progn ,@(mapcar (lambda (x) `(var ,(rename x)))
                                     dynamic-bindings)
                           (try (progn
                                  (setf ,@(loop for x in dynamic-bindings append
                                               `(,(rename x) ,(var x)
                                                  ,(var x) ,(val x))))
                                  ,renamed-body)
                                (:finally
                                 (setf ,@(mapcan (lambda (x) `(,(var x) ,(rename x)))
                                                 dynamic-bindings)))))
                   renamed-body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iteration

(defun make-for-vars/inits (init-forms)
  (mapcar (lambda (x)
            (cons (ps-macroexpand (if (atom x) x (first x)))
                  (compile-expression (if (atom x) nil (second x)))))
          init-forms))

(defun compile-loop-body (loop-vars body)
  (let* ((in-loop-scope? t)
         (*loop-scope-lexicals* loop-vars)
         (*loop-scope-lexicals-captured* ())
         (*ps-gensym-counter* *ps-gensym-counter*)
         (compiled-body (compile-statement `(progn ,@body))))
    (aif (remove-duplicates *loop-scope-lexicals-captured*)
         `(js:block
              (js:with ,(compile-expression
                         `(create ,@(loop for x in it
                                          collect x
                                          collect (when (member x loop-vars) x))))
                ,compiled-body))
         compiled-body)))

(define-statement-operator for (init-forms cond-forms step-forms &body body)
  (let ((init-forms (make-for-vars/inits init-forms)))
   `(js:for ,init-forms
            ,(mapcar #'compile-expression cond-forms)
            ,(mapcar #'compile-expression step-forms)
            ,(compile-loop-body (mapcar #'car init-forms) body))))

(define-statement-operator for-in ((var object) &rest body)
  `(js:for-in ,(compile-expression var)
              ,(compile-expression object)
              ,(compile-loop-body (list var) body)))

(define-statement-operator while (test &rest body)
  `(js:while ,(compile-expression test)
     ,(compile-loop-body () body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc

(define-statement-operator with (expression &rest body) ;; this should be deprecated
  `(js:with ,(compile-expression expression)
     ,(compile-statement `(progn ,@body))))

(define-statement-operator try (form &rest clauses)
  (let ((catch (cdr (assoc :catch clauses)))
        (finally (cdr (assoc :finally clauses))))
    (assert (not (cdar catch)) () "Sorry, currently only simple catch forms are supported.")
    (assert (or catch finally) () "Try form should have either a catch or a finally clause or both.")
    `(js:try ,(compile-statement `(progn ,form))
             :catch ,(when catch (list (caar catch) (compile-statement `(progn ,@(cdr catch)))))
             :finally ,(when finally (compile-statement `(progn ,@finally))))))

(define-expression-operator regex (regex)
  `(js:regex ,(string regex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalutation

(define-expression-operator quote (x)
  (flet ((quote% (expr) (when expr `',expr)))
    (compile-expression
     (typecase x
       (cons `(array ,@(mapcar #'quote% x)))
       (null '(array))
       (keyword x)
       (symbol (symbol-to-js-string x))
       (number x)
       (string x)
       (vector `(array ,@(loop for el across x collect (quote% el))))))))

(define-expression-operator lisp (lisp-form)
  ;; (ps (foo (lisp bar))) is like (ps* `(foo ,bar))
  ;; When called from inside of ps*, lisp-form has access to the
  ;; dynamic environment only, analogous to eval.
  `(js:escape
    (with-output-to-string (*psw-stream*)
      (let ((compile-expression? ,compile-expression?))
        (parenscript-print (ps-compile ,lisp-form) t)))))

(define-expression-operator eval-when (situation-list &body body)
  "The body is evaluated only during the given situations. The
accepted situations are :load-toplevel, :compile-toplevel,
and :execute. The code in BODY is assumed to be Common Lisp code
in :compile-toplevel and :load-toplevel sitations, and Parenscript
code in :execute."
  (when (and (member :compile-toplevel situation-list)
	     (member *compilation-level* '(:toplevel :inside-toplevel-form)))
    (eval `(progn ,@body)))
  (if (member :execute situation-list)
      (ps-compile `(progn ,@body))
      (ps-compile `(progn))))
