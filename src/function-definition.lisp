(in-package #:parenscript)
(in-readtable :parenscript)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lambda lists

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
  "The lambda list is transformed as follows:

* standard and optional variables are the mapped directly into
  the js-lambda list

* keyword variables are not included in the js-lambda list, but
  instead are obtained from the magic js ARGUMENTS
  pseudo-array. Code assigning values to keyword vars is
  prepended to the body of the function."
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
                (let (defaults assigns)
                  (mapc
                   (lambda (k)
                     (multiple-value-bind (var init-form keyword-str suppl)
                         (parse-key-spec k)
                       (push `(var ,var ,@(when init-form `((if (undefined ,var) ,init-form ,var)))) defaults)
                       (when suppl (push `(var ,suppl) defaults))
                       (push `(,keyword-str
                               (setf ,var (aref arguments (1+ ,n))
                                     ,@(when suppl `(,suppl t))))
                             assigns)))
                   (reverse keys))
                  `((loop for ,n from ,(length requireds) below (length arguments) by 2 do
                         (case (aref arguments ,n)
                           ,@assigns))
                    ,@defaults)))))
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
           (docstring (and (cdr body) (stringp (car body)) (car body)))
           (effective-body (append opt-forms
                                   key-forms
                                   (awhen rest-form (list it))
                                   (if docstring (rest body) body))))
      (values effective-args effective-body docstring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common

(defun collapse-function-return-blocks (body)
  (append (butlast body)
          (let ((last (ps-macroexpand (car (last body)))))
            (if (and (listp last) (eq 'block (car last)))
                (progn (push (or (second last) 'nilBlock) *function-block-names*)
                       (cddr last))
                (list last)))))

(defun compile-function-body (args body)
  (with-declaration-effects (body body)
    (let* ((*enclosing-lexical-block-declarations* ())
           (*enclosing-lexicals*                   (set-difference *enclosing-lexicals* args))
           (body                                   (let ((in-loop-scope?                 nil)
                                                         (*loop-scope-lexicals*          ())
                                                         (*loop-scope-lexicals-captured* ()))
                                                     (compile-statement `(return-from %function-body
                                                                           (progn ,@(collapse-function-return-blocks body))))))
           (var-decls                              (compile-statement
                                                    `(progn ,@(mapcar (lambda (var) `(var ,var))
                                                                      (remove-duplicates *enclosing-lexical-block-declarations*))))))
      (when in-loop-scope? ;; this is probably broken when it comes to let-renaming
        (setf *loop-scope-lexicals-captured* (append (intersection (flatten body) *loop-scope-lexicals*)
                                                     *loop-scope-lexicals-captured*)))
      `(ps-js:block ,@(cdr var-decls) ,@(cdr body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lambda

(define-expression-operator lambda (lambda-list &rest body)
  (multiple-value-bind (effective-args effective-body) (parse-extended-function lambda-list body)
    `(ps-js:lambda ,effective-args
       ,(let ((*function-block-names* nil)
              (*dynamic-extent-return-tags* (append *function-block-names*
                                                    *lexical-extent-return-tags*
                                                    *dynamic-extent-return-tags*))
              (*lexical-extent-return-tags* ()))
          (compile-function-body effective-args effective-body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; named functions

(defun compile-named-function-body (name lambda-list body)
  (let ((*enclosing-lexicals* (cons name *enclosing-lexicals*))
        (*function-block-names* (list name))
        (*lexical-extent-return-tags* ())
        (*dynamic-extent-return-tags* ())
        (*tags-that-return-throws-to* ()))
    (multiple-value-bind (effective-args effective-body docstring)
        (parse-extended-function lambda-list body)
      (values effective-args
              (wrap-block-for-dynamic-return name
                (compile-function-body effective-args effective-body))
              docstring))))

(define-statement-operator defun% (name lambda-list &rest body)
  (multiple-value-bind (effective-args body-block docstring)
      (compile-named-function-body name lambda-list body)
    (list 'ps-js:defun name effective-args docstring body-block)))

(defun maybe-rename-local-function (fun-name)
  (or (getf *local-function-names* fun-name) fun-name))

(defun collect-function-names (fn-defs)
  (loop for (fn-name) in fn-defs
        collect fn-name
        collect (if (or (member fn-name *enclosing-lexicals*) (lookup-macro-def fn-name *symbol-macro-env*))
                    (ps-gensym (string fn-name))
                    fn-name)))

(define-expression-operator flet (fn-defs &rest body)
  (let* ((fn-renames                 (collect-function-names fn-defs))
         ;; the function definitions need to be compiled with previous lexical bindings
         (fn-defs                    (loop for (fn-name . (args . body)) in fn-defs collect
                                          (progn (when compile-expression?
                                                   (push (getf fn-renames fn-name) *enclosing-lexical-block-declarations*))
                                                 (list (if compile-expression? 'ps-js:= 'ps-js:var)
                                                       (getf fn-renames fn-name)
                                                       (multiple-value-bind (args1 body-block)
                                                           (compile-named-function-body fn-name args body)
                                                         `(ps-js:lambda ,args1 ,body-block))))))
         ;; the flet body needs to be compiled with the extended lexical environment
         (*enclosing-lexicals*       (append fn-renames *enclosing-lexicals*))
         (*loop-scope-lexicals*      (when in-loop-scope? (append fn-renames *loop-scope-lexicals*)))
         (*local-function-names*     (append fn-renames *local-function-names*)))
    `(,(if compile-expression? 'ps-js:|,| 'ps-js:block)
       ,@fn-defs
       ,@(compile-progn body))))

(define-expression-operator labels (fn-defs &rest body)
  (let* ((fn-renames                 (collect-function-names fn-defs))
         (*local-function-names*     (append fn-renames *local-function-names*))
         (*enclosing-lexicals*       (append fn-renames *enclosing-lexicals*))
         (*loop-scope-lexicals*      (when in-loop-scope? (append fn-renames *loop-scope-lexicals*))))
    `(,(if compile-expression? 'ps-js:|,| 'ps-js:block)
       ,@(loop for (fn-name . (args . body)) in fn-defs collect
                    (progn (when compile-expression?
                             (push (getf *local-function-names* fn-name) *enclosing-lexical-block-declarations*))
                           (list (if compile-expression? 'ps-js:= 'ps-js:var)
                                 (getf *local-function-names* fn-name)
                                 (let ((*function-block-names* (list fn-name)))
                                   (compile-expression `(lambda ,args ,@body))))))
       ,@(compile-progn body))))

(define-expression-operator function (fn-name) ;; one of the things responsible for function namespace
  (ps-compile (maybe-rename-local-function fn-name)))
