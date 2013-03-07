(in-package #:parenscript)
(in-readtable :parenscript)

(define-condition simple-style-warning (style-warning simple-warning)
  ())

(defun warn-deprecated (old-name &optional new-name)
  (unless *suppress-deprecation*
    (warn 'simple-style-warning
          :format-control "~:@(~a~) is deprecated~:[.~;, use ~:@(~a~) instead~]"
          :format-arguments (list old-name new-name new-name))))

(defmacro defun-js (old-name new-name args &body body)
  `(defun ,old-name ,args
    ,(when (and (stringp (car body)) (< 1 (length body))) ; docstring
           (car body))
    (warn-deprecated ',old-name ',new-name)
    ,@body))

;;; DEPRECATED INTERFACE

(defmacro define-script-symbol-macro (name &body body)
  (warn-deprecated 'define-script-symbol-macro 'define-ps-symbol-macro)
  `(define-ps-symbol-macro ,name ,@body))

(defun js-equal (ps-form1 ps-form2)
  (warn-deprecated 'js-equal)
  (equalp ps-form1 ps-form2))

(defun-js js-compile compile-script (form)
  (compile-script form))

(defun-js js-compile-list compile-script (form)
  (compile-script form))

(defmacro defjsmacro (&rest args)
  (warn-deprecated 'defjsmacro 'defpsmacro)
  `(defpsmacro ,@args))

(defmacro js-inline (&rest body)
  (warn-deprecated 'js-inline 'ps-inline)
  `(js-inline* '(progn ,@body)))

(defun-js js-inline* ps-inline* (&rest body)
  (apply #'ps-inline* body))

(defmacro with-unique-js-names (&rest args)
  (warn-deprecated 'with-unique-js-names 'with-ps-gensyms)
  `(with-ps-gensyms ,@args))

(defun-js gen-js-name ps-gensym (&optional (prefix "_JS_"))
  (ps-gensym prefix))

(defmacro js (&rest args)
  (warn-deprecated 'js 'ps)
  `(ps ,@args))

(defun-js js* ps* (&rest args)
  (apply #'ps* args))

(defun-js compile-script ps* (ps-form &key (output-stream nil))
  "Compiles the Parenscript form PS-FORM into Javascript.
If OUTPUT-STREAM is NIL, then the result is a string; otherwise code
is output to the OUTPUT-STREAM stream."
  (format output-stream "~A" (ps* ps-form)))

(defun-js symbol-to-js symbol-to-js-string (symbol)
  (symbol-to-js-string symbol))

(defmacro defmacro/ps (name args &body body)
  (warn-deprecated 'defmacro/ps 'defmacro+ps)
  `(progn (defmacro ,name ,args ,@body)
          (import-macros-from-lisp ',name)))

(defmacro defpsmacro-deprecated (old new)
  `(defpsmacro ,old (&rest args)
     (warn-deprecated ',old ',new)
     (cons ',new args)))

(defpsmacro-deprecated slot-value getprop)
(defpsmacro-deprecated === eql)
(defpsmacro-deprecated == equal)
(defpsmacro-deprecated % rem)
(defpsmacro-deprecated concat-string stringify)

(defpsmacro !== (&rest args)
  (warn-deprecated '!==)
  `(not (eql ,@args)))

(defpsmacro != (&rest args)
  (warn-deprecated '!=)
  `(not (equal ,@args)))

(defpsmacro labeled-for (label init-forms cond-forms step-forms &rest body)
  (warn-deprecated 'labeled-for 'label)
  `(label ,label (for ,init-forms ,cond-forms ,step-forms ,@body)))

(defpsmacro do-set-timeout ((timeout) &body body)
  (warn-deprecated 'do-set-timeout 'set-timeout)
  `(set-timeout (lambda () ,@body) ,timeout))

(defun concat-string (&rest things)
  (warn-deprecated 'concat-string 'stringify)
  (apply #'stringify things))

(define-statement-operator with (expression &rest body)
  (warn-deprecated 'with '|LET or WITH-SLOTS|)
  `(ps-js:with ,(compile-expression expression)
     ,(compile-statement `(progn ,@body))))

(defpsmacro label (&rest args)
  (warn-deprecated 'label 'block)
  `(block ,@args))

(define-ps-symbol-macro f ps-js:false)

;;; bind and bind*

(defun dot->rest (x)
  (cond ((atom x) x)
        ((not (listp (cdr x)))        ; dotted list
         (list (dot->rest (car x)) '&rest (dot->rest (cdr x))))
        (t (cons (dot->rest (car x)) (dot->rest (cdr x))))))

(defun property-bindings-p (x)
  (when (consp x)
    (every (lambda (y)
             (or (keywordp y)
                 (and (consp y)
                      (= (length y) 2)
                      (symbolp (car y))
                      (not (keywordp (car y)))
                      (keywordp (cadr y)))))
           x)))

(defun extract-bindings (x)
  ;; returns a pair of destructuring bindings and property bindings
  (cond ((atom x) (list x nil))
        ((property-bindings-p x)
         (let ((var (ps-gensym)))
           (list var (list x var))))
        (t (loop :for y :on x
              :for (d p) = (extract-bindings (car y))
              :append (list d) :into ds
              :when p :append p :into ps
              :finally (return (list ds ps))))))

(defun property-bindings (bindings expr body)
  `(let ,(loop :for b :in bindings
            :for (var p) = (if (consp b) b (list (intern (string b)) b))
            :collect `(,var (@ ,expr ,p)))
     ,@body))

(defpsmacro bind (bindings expr &body body)
  (warn-deprecated 'bind)
  (setf bindings (dot->rest bindings))
  (destructuring-bind (d p)
      (extract-bindings bindings)
    (cond ((and (atom d)
                (or (= (length bindings) 1)
                    (atom expr)
                    (atom (ps-macroexpand expr))))
           (property-bindings bindings expr body))
          ((atom d)
           (with-ps-gensyms (var)
             `(let ((,var ,expr))
                (bind ,bindings ,var ,@body))))
          ((null p) `(destructuring-bind ,bindings ,expr ,@body))
          (t `(destructuring-bind ,d ,expr
                (bind* ,p ,@body))))))

(defpsmacro bind* (bindings &body body)
  (warn-deprecated 'bind*)
  (cond ((= (length bindings) 2)
         `(bind ,(car bindings) ,(cadr bindings) ,@body))
        (t `(bind ,(car bindings) ,(cadr bindings)
              (bind* ,(cddr bindings) ,@body)))))

(setf %compiling-reserved-forms-p% nil)
