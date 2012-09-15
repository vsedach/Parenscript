(in-package #:parenscript)
(in-readtable :parenscript)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; arithmetic and logic

(define-trivial-special-ops
  +          ps-js:+
  -          ps-js:-
  *          ps-js:*
  rem        ps-js:%
  and        ps-js:&&
  or         ps-js:\|\|

  logand     ps-js:&
  logior     ps-js:\|
  logxor     ps-js:^
  lognot     ps-js:~

  aref       ps-js:aref

  funcall    ps-js:funcall
  )

(define-expression-operator / (&rest args)
  `(ps-js:/ ,@(unless (cdr args) (list 1)) ,@(mapcar #'compile-expression args)))

(define-expression-operator - (&rest args)
  (let ((args (mapcar #'compile-expression args)))
    (cons (if (cdr args) 'ps-js:- 'ps-js:negate) args)))

(defun fix-nary-comparison (operator objects)
  (let* ((tmp-var-forms (butlast (cdr objects)))
         (tmp-vars (loop repeat (length tmp-var-forms)
                         collect (ps-gensym "_CMP")))
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
    <     ps-js:<
    >     ps-js:>
    <=    ps-js:<=
    >=    ps-js:>=
    eql   ps-js:===
    equal ps-js:==))

(define-expression-operator /= (a b)
  ;; for n>2, /= is finding duplicates in an array of numbers (ie -
  ;; nontrivial runtime algorithm), so we restrict it to binary in PS
  `(ps-js:!== ,(compile-expression a) ,(compile-expression b)))

(defun references? (exp place)
  (cond ((not exp) nil)
        ((atom exp) (equal exp place))
        (t (or (equal exp place)
               (references? (car exp) place)
               (references? (cdr exp) place)))))

(defmacro inc-dec (op op1 op2)
  `(let ((delta (ps-macroexpand delta)))
     (cond ((eql delta 1)
            (list ',op1 (compile-expression x)))
           ((references? delta x)
            (ps-compile
             (let ((var (ps-gensym "_PS_INCR_PLACE")))
               `(let ((,var ,delta))
                  (,',op ,x ,var)))))
           (t
            (list ',op2 (compile-expression x)
                  (compile-expression delta))))))

(define-expression-operator incf (x &optional (delta 1))
  (inc-dec incf ps-js:++ ps-js:+=))

(define-expression-operator decf (x &optional (delta 1))
  (inc-dec decf ps-js:-- ps-js:-=))

(let ((inverses (mapcan (lambda (x)
                          (list x (reverse x)))
                        '((ps-js:=== ps-js:!==)
                          (ps-js:== ps-js:!=)
                          (ps-js:< ps-js:>=)
                          (ps-js:> ps-js:<=)))))
  (define-expression-operator not (x)
    (let ((form (compile-expression x)))
      (acond ((and (listp form) (eq (car form) 'ps-js:!))
              (second form))
             ((and (listp form) (cadr (assoc (car form) inverses)))
              `(,it ,@(cdr form)))
             (t `(ps-js:! ,form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; blocks and control flow

(defun flatten-blocks (body)
  (when body
    (if (and (listp (car body)) (eq 'ps-js:block (caar body)))
        (append (cdr (car body)) (flatten-blocks (cdr body)))
        (cons (car body) (flatten-blocks (cdr body))))))

(defun compile-progn (body)
  (let ((block (flatten-blocks (mapcar #'ps-compile body))))
    (append (remove-if #'constantp (butlast block))
            (unless (and (or (eq *compilation-level* :toplevel)
                             (not compile-expression?))
                         (not (car (last block))))
              (last block)))))

(define-expression-operator progn (&rest body)
  (if (cdr body)
      `(ps-js:|,| ,@(compile-progn body))
      (compile-expression (car body))))

(define-statement-operator progn (&rest body)
  `(ps-js:block ,@(compile-progn body)))

(defun fill-mv-reg (values)
  `(setf __PS_MV_REG (create :tag    (@ arguments callee)
                             :values ,values)))

(defvar suppress-values?)

(defun wrap-for-dynamic-return (handled-tags body)
  (aif (loop for (tag . thrown?) in *dynamic-return-tags*
             when (and thrown? (member tag handled-tags))
             collect tag)
       (with-ps-gensyms (_ps_err)
         (flet ((make-catch-clause (tag)
                  `((and ,_ps_err (eql ',tag (getprop ,_ps_err :__ps_block_tag)))
                    ,(fill-mv-reg `(getprop ,_ps_err :__ps_values))
                    (return-from ,tag (getprop ,_ps_err :__ps_value1)))))
           `(ps-js:block
               (ps-js:try
                ,body
                :catch   (,_ps_err
                          ,(let ((suppress-values? nil))
                             (compile-statement
                              `(progn (cond
                                        ,@(mapcar #'make-catch-clause it)
                                        (t (throw ,_ps_err)))))))
                :finally nil))))
       body))

(define-statement-operator block (name &rest body)
  (if in-function-scope?
      (let* ((name                  (or name 'nilBlock))
             (in-loop-scope?        (if name in-loop-scope? nil))
             (*dynamic-return-tags* (cons (cons name nil) *dynamic-return-tags*))
             (*current-block-tag*   name)
             (compiled-body         (wrap-for-dynamic-return
                                     (list name)
                                     (compile-statement `(progn ,@body)))))
        (if (tree-search `(ps-js:break ,name) compiled-body)
            `(ps-js:label ,name ,compiled-body)
            compiled-body))
      (ps-compile (with-lambda-scope `(block ,name ,@body)))))

(defun return-exp (tag &optional (value nil value?) rest-values)
  (flet ((ret1only ()
           (let ((ret `(ps-js:return
                         ,@(when value?
                                 (list (compile-expression value))))))
             (if suppress-values?
                 `(ps-js:block (ps-js:= __PS_MV_REG {})
                               ,ret)
                 ret)))
         (fill-mv ()
           (list (fill-mv-reg `(list ,@rest-values)))))
    (acond ((eql tag *current-block-tag*)
            (compile-statement
             (if value?
                 `(progn ,value
                         ,@(when rest-values (fill-mv))
                         (break ,tag))
                 `(break ,tag))))
           ((or (eql '%function tag)
                (member tag *function-block-names*))
            (if rest-values
                (let* ((cvalue (compile-expression value))
                       (val1   (unless (or (constantp cvalue)
                                           (symbolp   cvalue))
                                 (ps-gensym "VAL1_"))))
                  (let ((suppress-values? nil))
                    (compile-statement
                     `(let ,(when val1 `((,val1 ,value)))
                        ,@(fill-mv)
                        (return-from ,tag ,(or val1 value))))))
                (ret1only)))
           ((assoc tag *dynamic-return-tags*)
            (setf (cdr it) t)
            (ps-compile
             `(throw (create
                      :__ps_block_tag      ',tag
                      :__ps_value1          ,value
                      ,@(when rest-values
                         `(:__ps_values (list ,@rest-values)))))))
           (t
            (warn "Returning from unknown block ~A" tag)
            (ret1only))))) ;; for backwards-compatibility

(defun try-expressionizing-if? (exp &optional (score 0)) ;; poor man's codewalker
  "Heuristic that tries not to expressionize deeply nested if expressions."
  (cond ((< 1 score) nil)
        ((and (listp exp) (eq (car exp) 'quote))
         t)
        ((listp exp)
         (loop for x in (cdr exp) always
              (try-expressionizing-if?
               (or (ignore-errors (ps-macroexpand x)) x) ;; fail
               (+ score (case (car exp)
                          ((if cond let) 1)
                          ((progn) (1- (length (cdr exp))))
                          (otherwise 0))))))
        (t t)))

(defun expressionize-result (tag form)
  (ps-compile
   (case (car form)
     ((continue break throw) ;; non-local exit
      form)
     ;; implicit progn forms
     ((with label let flet labels macrolet symbol-macrolet block)
      `(,(first form) ,(second form)
         ,@(butlast (cddr form))
         (return-from ,tag ,(car (last (cddr form))))))
     (progn
       `(progn ,@(butlast (cdr form))
               (return-from ,tag ,(car (last (cdr form))))))
     (switch
      `(switch
        ,(second form)
        ,@(loop for (cvalue . cbody) in (cddr form)
             for remaining on (cddr form) collect
               (aif (cond ((or (eq 'default cvalue) (not (cdr remaining)))
                           1)
                          ((eq 'break (car (last cbody)))
                           2))
                    (let ((result-form (ps-macroexpand (car (last cbody it)))))
                      `(,cvalue
                        ,@(butlast cbody it)
                        (return-from ,tag
                          ,(if (eq result-form 'break) nil result-form))))
                    (cons cvalue cbody)))))
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
       `(cond
          ,@(loop for clause in (cdr form) collect
                 `(,@(butlast clause) (return-from ,tag ,(car (last clause)))))
          ,@(when in-case? `((t (return-from ,tag nil))))))
     (if
      (if (and (try-expressionizing-if? form)
               (not (find 'values (flatten form)))
               (let ((used-up-names                   *used-up-names*)
                     (*lambda-wrappable-statements*                ()))
                 (handler-case (compile-expression form)
                   (compile-expression-error ()
                     (setf *used-up-names* used-up-names)
                     nil))))
           (return-from expressionize-result (return-exp tag form))
           `(if ,(second form)
                (return-from ,tag ,(third form))
                ,@(when (or in-case? (fourth form))
                   `((return-from ,tag ,(fourth form)))))))
     (return-from ;; this will go away someday
      (unless tag
        (warn 'simple-style-warning
              :format-control "Trying to RETURN a RETURN without a block tag specified. Perhaps you're still returning values from functions by hand?
Parenscript now implements implicit return, update your code! Things like (lambda () (return x)) are not valid Common Lisp and may not be supported in future versions of Parenscript."))
       form)
     (otherwise
      (return-from expressionize-result
        (cond ((not (gethash (car form) *special-statement-operators*))
               (return-exp tag form))
              (in-case?
               `(ps-js:block ,(compile-statement form) ,(return-exp tag)))
              (t (compile-statement form))))))))

(define-statement-operator return-from (tag &optional result)
  (if tag
      (let ((form (ps-macroexpand result)))
        (cond ((atom form)             (return-exp tag form))
              ((eq 'values (car form)) (return-exp tag (cadr form) (cddr form)))
              (t                       (expressionize-result tag form))))
      (ps-compile `(return-from nilBlock ,result))))


(define-expression-operator values (&optional main &rest additional)
  (when main
    (ps-compile (if additional
                    `(prog1 ,main ,@additional)
                    main))))

(define-statement-operator throw (&rest args)
  `(ps-js:throw ,@(mapcar #'compile-expression args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conditionals

(define-expression-operator if (test then &optional else)
   `(ps-js:? ,(compile-expression test)
             ,(compile-expression then)
             ,(compile-expression else)))

(define-statement-operator if (test then &optional else)
  `(ps-js:if ,(compile-expression test)
             ,(compile-statement `(progn ,then))
             ,@(when else
                     `(:else ,(compile-statement `(progn ,else))))))

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
  `(ps-js:if ,(compile-expression (caar clauses))
             ,(compile-statement `(progn ,@(cdar clauses)))
             ,@(loop for (test . body) in (cdr clauses) appending
                    (if (eq t test)
                        `(:else ,(compile-statement `(progn ,@body)))
                        `(:else-if ,(compile-expression test)
                                   ,(compile-statement `(progn ,@body)))))))

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
;;; assignment

(defun assignment-op (op)
  (getf '(ps-js:+   ps-js:+=
          ps-js:~   ps-js:~=
          ps-js:&   ps-js:&=
          ps-js:\|  ps-js:\|=
          ps-js:-   ps-js:-=
          ps-js:*   ps-js:*=
          ps-js:%   ps-js:%=
          ps-js:>>  ps-js:>>=
          ps-js:^   ps-js:^=
          ps-js:<<  ps-js:<<=
          ps-js:>>> ps-js:>>>=
          ps-js:/   ps-js:/=)
        op))

(define-expression-operator ps-assign (lhs rhs)
  (let ((rhs (ps-macroexpand rhs)))
    (if (and (listp rhs) (eq (car rhs) 'progn))
        (ps-compile `(progn ,@(butlast (cdr rhs))
                            (ps-assign ,lhs ,(car (last (cdr rhs))))))
        (let ((lhs (compile-expression lhs))
              (rhs (compile-expression rhs)))
          (aif (and (listp rhs)
                    (= 3 (length rhs))
                    (equal lhs (second rhs))
                    (assignment-op (first rhs)))
               (list it lhs (if (fourth rhs)
                                (cons (first rhs) (cddr rhs))
                                (third rhs)))
               (list 'ps-js:= lhs rhs))))))

(define-statement-operator defvar (name &optional
                                        (value (values) value-provided?)
                                        documentation)
  ;; this must be used as a top-level form, otherwise the resulting
  ;; behavior will be undefined.
  (declare (ignore documentation))
  (pushnew name *special-variables*)
  (ps-compile `(var ,name ,@(when value-provided? (list value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; binding

(defmacro with-declaration-effects ((var block) &body body)
  (let ((declarations (gensym)))
   `(let* ((,var ,block)
           (,declarations (and (listp (car ,var))
                               (eq (caar ,var) 'declare)
                               (cdar ,var)))
           (,var (if ,declarations
                     (cdr ,var)
                     ,var))
           (*special-variables* (append (cdr (find 'special ,declarations :key #'car)) *special-variables*)))
      ,@body)))

(defun maybe-rename-lexical-var (x symbols-in-bindings)
  (when (or (member x *enclosing-lexicals*)
            (member x *enclosing-function-arguments*)
            (when (boundp '*used-up-names*)
              (member x *used-up-names*))
            (lookup-macro-def x *symbol-macro-env*)
            (member x symbols-in-bindings))
    (ps-gensym (symbol-name x))))

(defun with-lambda-scope (body)
 (prog1 `((lambda () ,body))
   (setf *vars-needing-to-be-declared* ())))

(define-expression-operator let (bindings &body body)
  (with-declaration-effects (body body)
    (flet ((rename (x) (first x))
           (var (x) (second x))
           (val (x) (third x)))
      (let* ((new-lexicals ())
             (normalized-bindings
              (mapcar (lambda (x)
                        (if (symbolp x)
                            (list x nil)
                            (list (car x) (ps-macroexpand (cadr x)))))
                      bindings))
             (symbols-in-bindings
              (mapcan (lambda (x) (flatten (cadr x)))
                      normalized-bindings))
             (lexical-bindings
              (loop for x in normalized-bindings
                    unless (special-variable? (car x)) collect
                    (cons (aif (maybe-rename-lexical-var (car x)
                                                         symbols-in-bindings)
                               it
                               (progn
                                 (push (car x) new-lexicals)
                                 (when (boundp '*used-up-names*)
                                   (push (car x) *used-up-names*))
                                 nil))
                          x)))
             (dynamic-bindings
              (loop for x in normalized-bindings
                    when (special-variable? (car x)) collect
                    (cons (ps-gensym (format nil "~A_~A" (car x) 'tmp-stack))
                          x)))
             (renamed-body
              `(symbol-macrolet ,(loop for x in lexical-bindings
                                       when (rename x) collect
                                       `(,(var x) ,(rename x)))
                 ,@body))
             (*enclosing-lexicals*
              (append new-lexicals *enclosing-lexicals*))
             (*loop-scope-lexicals*
              (when in-loop-scope?
                (append new-lexicals *loop-scope-lexicals*)))
             (let-body
              `(progn
                 ,@(mapcar (lambda (x)
                             `(var ,(or (rename x) (var x)) ,(val x)))
                           lexical-bindings)
                 ,(if dynamic-bindings
                      `(progn
                         ,@(mapcar (lambda (x) `(var ,(rename x)))
                                   dynamic-bindings)
                         (try
                          (progn
                            (setf ,@(loop for x in dynamic-bindings append
                                         `(,(rename x) ,(var x)
                                            ,(var x) ,(val x))))
                            ,renamed-body)
                          (:finally
                           (setf ,@(mapcan (lambda (x) `(,(var x) ,(rename x)))
                                           dynamic-bindings)))))
                      renamed-body))))
        (ps-compile (cond (in-function-scope? let-body)
                          ;; HACK
                          ((find-if (lambda (x)
                                      (member x '(defun% defvar)))
                                    (flatten
                                     (loop for x in body collecting
                                          (or (ignore-errors (ps-macroexpand x))
                                              x))))
                           let-body)
                          (t (with-lambda-scope let-body))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iteration

(defun make-for-vars/inits (init-forms)
  (mapcar (lambda (x)
            (cons (ps-macroexpand (if (atom x) x (first x)))
                  (compile-expression (if (atom x) nil (second x)))))
          init-forms))

(defun compile-loop-body (loop-vars body)
  (let* ((in-loop-scope?                                                    t)
         ;; provides lexical bindings for all free variables using WITH
         (in-function-scope?                                                t)
         (*loop-scope-lexicals*                                     loop-vars)
         (*loop-scope-lexicals-captured*                                   ())
         (*ps-gensym-counter*                             *ps-gensym-counter*)
         (compiled-body                   (compile-statement `(progn ,@body))))
    ;; the sort is there to make order for output-tests consistent across implementations
    (aif (sort (remove-duplicates *loop-scope-lexicals-captured*)
               #'string< :key #'symbol-name)
         `(ps-js:block
              (ps-js:with
                  ,(compile-expression
                    `(create
                      ,@(loop for x in it
                              collect x
                              collect (when (member x loop-vars) x))))
                ,compiled-body))
         compiled-body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evalutation

(define-expression-operator quote (x)
  (flet ((quote% (expr) (when expr `',expr)))
    (compile-expression
     (typecase x
       (cons `(array ,@(mapcar #'quote% x)))
       ((or null (eql [])) '(array))
       (keyword x)
       (symbol (symbol-to-js-string x))
       (number x)
       (string x)
       (vector `(array ,@(loop for el across x collect (quote% el))))))))

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
