(in-package "PARENSCRIPT")

(define-ps-symbol-macro f js:f)
(define-ps-symbol-macro false js:f)

(macrolet ((define-trivial-mappings (&rest mappings)
             `(progn
                ,@(loop for (macro-name ps-op) on mappings by #'cddr collect
                       `(defpsmacro ,macro-name (&rest args)
                          (cons ',ps-op args))))))
  (define-trivial-mappings
    string= eql
    eq      eql
    =       eql
    list    array
    elt     aref))

(defpsmacro null (x)
  `(equal ,x nil))

;;; Math

(defmacro def-js-maths (&rest mathdefs)
  `(progn ,@(mapcar (lambda (def) (cons 'defpsmacro def)) mathdefs)))

(def-js-maths
    (max (&rest nums) `((@ *math max) ,@nums))
    (min (&rest nums) `((@ *math min) ,@nums))
    (floor (n &optional divisor) `((@ *math floor) ,(if divisor `(/ ,n ,divisor) n)))
    (ceiling (n &optional divisor) `((@ *math ceil) ,(if divisor `(/ ,n ,divisor) n)))
    (round (n &optional divisor) `((@ *math round) ,(if divisor `(/ ,n ,divisor) n)))
    (sin (n) `((@ *math sin) ,n))
    (cos (n) `((@ *math cos) ,n))
    (tan (n) `((@ *math tan) ,n))
    (asin (n) `((@ *math asin) ,n))
    (acos (n) `((@ *math acos) ,n))
    (atan (y &optional x) (if x `((@ *math atan2) ,y ,x) `((@ *math atan) ,y)))
    (sinh (n) `((lambda (x) (return (/ (- (exp x) (exp (- x))) 2))) ,n))
    (cosh (n) `((lambda (x) (return (/ (+ (exp x) (exp (- x))) 2))) ,n))
    (tanh (n) `((lambda (x) (return (/ (- (exp x) (exp (- x))) (+ (exp x) (exp (- x)))))) ,n))
    (asinh (n) `((lambda (x) (return (log (+ x (sqrt (1+ (* x x))))))) ,n))
    (acosh (n) `((lambda (x) (return (* 2 (log (+ (sqrt (/ (1+ x) 2)) (sqrt (/ (1- x) 2))))))) ,n))
    (atanh (n) `((lambda (x) (return (/ (- (log (+ 1 x)) (log (- 1 x))) 2))) ,n))
    (1+ (n) `(+ ,n 1))
    (1- (n) `(- ,n 1))
    (abs (n) `((@ *math abs) ,n))
    (evenp (n) `(not (oddp ,n)))
    (oddp (n) `(% ,n 2))
    (exp (n) `((@ *math exp) ,n))
    (expt (base power) `((@ *math pow) ,base ,power))
    (log (n &optional base)
      (or (and (null base) `((@ *math log) ,n))
          (and (numberp base) (= base 10) `(* (log ,n) (@ *math *log10e*)))
          `(/ (log ,n) (log ,base))))
    (sqrt (n) `((@ *math sqrt) ,n))
    (random (&optional upto) (if upto
                                 `(floor (* ,upto ((@ *math random))))
                                 '((@ *math random)))))

(define-ps-symbol-macro pi (getprop *math '*pi*))

;;; Types

(defpsmacro stringp (x)
  `(string= (typeof ,x) "string"))

(defpsmacro numberp (x)
  `(string= (typeof ,x) "number"))

(defpsmacro functionp (x)
  `(string= (typeof ,x) "function"))

(defpsmacro objectp (x)
  `(string= (typeof ,x) "object"))

(defpsmacro undefined (x)
  `(eql undefined ,x))

(defpsmacro defined (x)
  `(not (undefined ,x)))

;;; Data structures

(defpsmacro [] (&rest args)
  `(array ,@(mapcar (lambda (arg)
                      (if (and (consp arg) (not (equal '[] (car arg))))
                          (cons '[] arg)
                          arg))
                    args)))

(defpsmacro make-array (&rest initial-values)
  `(new (*array ,@initial-values)))

(defpsmacro length (a)
  `(getprop ,a 'length))

;;; Getters

(defpsmacro getprop (obj &rest slots)
  (if (null (rest slots))
      `(%js-getprop ,obj ,(first slots))
      `(getprop (getprop ,obj ,(first slots)) ,@(rest slots))))

(defpsmacro @ (obj &rest props)
  "Handy getprop/aref composition macro."
  (if props
      `(@ (getprop ,obj ,(if (symbolp (car props))
                             `',(car props)
                             (car props)))
          ,@(cdr props))
      obj))

(defpsmacro chain (&rest method-calls)
  (labels ((do-chain (method-calls)
             (if (cdr method-calls)
                 (if (listp (car method-calls))
                     `((@ ,(do-chain (cdr method-calls)) ,(caar method-calls)) ,@(cdar method-calls))
                     `(@ ,(do-chain (cdr method-calls)) ,(car method-calls)))
                 (car method-calls))))
    (do-chain (reverse method-calls))))

(defpsmacro with-slots (slots object &rest body)
  (flet ((slot-var (slot)
           (if (listp slot)
               (first slot)
               slot))
         (slot-symbol (slot)
           (if (listp slot)
               (second slot)
               slot)))
    `(symbol-macrolet ,(mapcar (lambda (slot)
                                 `(,(slot-var slot) (getprop ,object ',(slot-symbol slot))))
                               slots)
       ,@body)))

;;; multiple values

(defpsmacro values (&optional main &rest additional)
  (when main
    (if additional
        (with-ps-gensyms (val1 valrest)
          `(let ((,val1 ,main)
                 (,valrest (list ,@additional)))
             (when (defined (@ arguments :callee :caller :mv))
               (setf (@ arguments :callee :caller :mv) ,valrest))
             ,val1))
        main)))

(defpsmacro multiple-value-bind (vars expr &body body)
  (let ((expr (ps-macroexpand expr)))
    (if (and (consp expr) (implicit-progn-form? expr))
        `(,@(butlast expr)
            (multiple-value-bind ,vars
                ,@(last expr)
              ,@body))
        (with-ps-gensyms (mv prev-mv)
          `(let ((,prev-mv (@ arguments :callee :mv)))
             (try
              (progn
                (setf (@ arguments :callee :mv) t)
                (let ((,(car vars) ,expr)
                      (,mv (if (objectp (@ arguments :callee :mv))
                               (@ arguments :callee :mv)
                               (make-array ,(1- (length vars))))))
                  (destructuring-bind ,(cdr vars) ,mv
                    ,@body)))
              (:finally (if (undefined ,prev-mv)
                            (delete (@ arguments :callee :mv))
                            (setf (@ arguments :callee :mv) ,prev-mv)))))))))

;;; conditionals

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
  `(if ,test
       (progn ,@body)))

(defpsmacro unless (test &rest body)
  `(when (not ,test)
     ,@body))

;;; function definition

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
      (progn (assert (and (listp name) (= (length name) 2) (eq 'setf (car name))) ()
                     "(defun ~s ~s ...) needs to have a symbol or (setf symbol) for a name." name lambda-list)
             `(defun-setf ,name ,lambda-list ,@body))))

(defpsmacro defun-function (name lambda-list &body body)
  (multiple-value-bind (effective-args effective-body)
      (parse-extended-function lambda-list body)
    `(%js-defun ,name ,effective-args
                ,@effective-body)))

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

;;; defining setf expanders

(defvar *defun-setf-name-prefix* "__setf_")

(defpsmacro defun-setf (setf-name lambda-list &body body)
  (let ((mangled-function-name
         (intern (concatenate 'string *defun-setf-name-prefix*
                              (symbol-name (second setf-name)))
                 (symbol-package (second setf-name)))))
    (setf (gethash (second setf-name) *ps-setf-expanders*)
          (compile
           nil
           (lambda (access-args store-form)
             `(,mangled-function-name ,store-form ,@access-args))))
    `(defun ,mangled-function-name ,lambda-list ,@body)))

;;; slightly broken WRT lambda lists
(defpsmacro defsetf-long (access-fn lambda-list (store-var) form)
  (setf (gethash access-fn *ps-setf-expanders*)
        (compile
         nil
         (let ((var-bindings (ordered-set-difference lambda-list
                                                     lambda-list-keywords)))
           `(lambda (access-fn-args store-form)
              (destructuring-bind ,lambda-list
                  access-fn-args
                (let* ((,store-var (ps-gensym))
                       (gensymed-names (loop repeat ,(length var-bindings)
                                          collecting (ps-gensym)))
                       (gensymed-arg-bindings (mapcar #'list
                                                      gensymed-names
                                                      (list ,@var-bindings))))
                  (destructuring-bind ,var-bindings
                      gensymed-names
                    `(let* (,@gensymed-arg-bindings
                            (,,store-var ,store-form))
                       ,,form))))))))
  nil)

(defpsmacro defsetf-short (access-fn update-fn &optional docstring)
  (declare (ignore docstring))
  (setf (gethash access-fn *ps-setf-expanders*)
        (lambda (access-fn-args store-form)
          `(,update-fn ,@access-fn-args ,store-form)))
  nil)

(defpsmacro defsetf (access-fn &rest args)
  `(,(if (= (length args) 3) 'defsetf-long 'defsetf-short) ,access-fn ,@args))

;;; setf

(defpsmacro setf (&rest args)
  (assert (evenp (length args)) ()
          "~s does not have an even number of arguments." `(setf ,args))
  `(progn ,@(loop for (place value) on args by #'cddr collect
                 (aif (and (listp place)
                           (gethash (car place) *ps-setf-expanders*))
                      (funcall it (cdr place) value)
                      `(ps-assign ,place ,value)))))

(defpsmacro psetf (&rest args)
  (let ((places (loop for x in args by #'cddr collect x))
        (vals (loop for x in (cdr args) by #'cddr collect x)))
    (let ((gensyms (loop repeat (length places) collect (ps-gensym))))
      `(let ,(mapcar #'list gensyms vals)
         (setf ,@(mapcan #'list places gensyms))))))

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

;;; iteration

(defun do-make-let-bindings (decls)
  (mapcar (lambda (x)
            (if (atom x)
                x
                (if (endp (cdr x))
                    (list (car x))
                    (subseq x 0 2))))
          decls))

(defun do-make-init-vars (decls)
  (mapcar (lambda (x)
            (if (atom x)
                x
                (first x)))
          decls))

(defun do-make-init-vals (decls)
  (mapcar (lambda (x)
            (if (or (atom x) (endp (cdr x)))
                nil
                (second x)))
          decls))

(defun do-make-for-vars/init (decls)
  (mapcar (lambda (x)
            (if (atom x) x
                (if (endp (cdr x)) x
                    (subseq x 0 2))))
          decls))

(defun do-make-for-steps (decls)
  (mapcar (lambda (x)
            `(setf ,(first x) ,(third x)))
          (remove-if (lambda (x)
                       (or (atom x) (< (length x) 3)))
                     decls)))

(defun do-make-iter-psteps (decls)
  `(psetq
    ,@(mapcan (lambda (x)
                (list (first x) (third x)))
              (remove-if (lambda (x)
                           (or (atom x) (< (length x) 3)))
                         decls))))

(defpsmacro do* (decls (termination &optional (result nil result?)) &body body)
  (if result?
      `((lambda ()
          (for ,(do-make-for-vars/init decls) ((not ,termination)) ,(do-make-for-steps decls)
               ,@body)
          (return ,result)))
      `(for ,(do-make-for-vars/init decls) ((not ,termination)) ,(do-make-for-steps decls)
            ,@body)))

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

(defpsmacro dotimes ((var count &optional (result nil result?)) &rest body)
  `(do* ((,var 0 (1+ ,var)))
        ((>= ,var ,count) ,@(when result? (list result)))
     ,@body))

(defpsmacro dolist ((var array &optional (result nil result?)) &body body)
  (let* ((idx (ps-gensym "_js_idx"))
         (introduce-array-var? (not (symbolp array)))
         (arrvar (if introduce-array-var?
                     (ps-gensym "_js_arrvar")
                     array)))
    `(do* (,var
           ,@(when introduce-array-var?
                   (list (list arrvar array)))
           (,idx 0 (1+ ,idx)))
          ((>= ,idx (getprop ,arrvar 'length))
           ,@(when result? (list result)))
       (setq ,var (aref ,arrvar ,idx))
       ,@body)))

;;; Concatenation

(defpsmacro concatenate (result-type &rest sequences)
  (assert (equal result-type ''string) () "Right now Parenscript 'concatenate' only support strings.")
  (cons '+ sequences))

(defmacro concat-string (&rest things)
  "Like concatenate but prints all of its arguments."
  `(format nil "~@{~A~}" ,@things))

(defpsmacro concat-string (&rest things)
  (cons '+ things))

(defpsmacro append (arr1 &rest arrs)
  (if arrs
      `((@ ,arr1 concat) ,@arrs)
      arr1))

;;; Destructuring bind

(defun destructuring-wrap (arr n bindings body &key setf?)
  (labels ((bind-expr (var expr inner-body)
             (if setf?
                 `(progn (setf ,var ,expr) ,inner-body)
                 `(let ((,var ,expr)) ,inner-body)))
           (bind-rest (sym)
             (bind-expr sym `(when (> (length ,arr) ,n)
                               ((@ ,arr slice) ,n))
                        body)))
    (cond ((null bindings)
           body)
          ((atom bindings) ;; dotted destructuring list
           (bind-rest bindings))
          ((eq (car bindings) '&rest)
           (if (and (= (length bindings) 2)
                    (atom (second bindings)))
               (bind-rest (second bindings))
               (error "~a is invalid in destructuring list." bindings)))
          ((eq (car bindings) '&optional)
           (destructuring-wrap arr n (cdr bindings) body :setf? setf?))
          (t (let ((var (car bindings))
                   (inner-body (destructuring-wrap arr (1+ n) (cdr bindings) body :setf? setf?)))
               (cond ((null var) inner-body)
                     ((atom var) (bind-expr var `(aref ,arr ,n) inner-body))
                     (t `(,(if setf? 'dset 'destructuring-bind)
                           ,var (aref ,arr ,n)
                           ,inner-body))))))))

(defpsmacro dset (bindings expr &body body)
  (let ((arr (if (complex-js-expr? expr) (ps-gensym) expr)))
    `(progn
       ,@(unless (eq arr expr) `((setf ,arr ,expr)))
       ,(destructuring-wrap arr 0 bindings (cons 'progn body) :setf? t))))

(defpsmacro destructuring-bind (bindings expr &body body)
  (let* ((arr (if (complex-js-expr? expr) (ps-gensym) expr))
         (bound (destructuring-wrap arr 0 bindings (cons 'progn body))))
    (if (eq arr expr)
        bound
        `(let ((,arr ,expr)) ,bound))))

;;; Control structures

(defpsmacro ignore-errors (&body body)
  `(try (progn ,@body) (:catch (e))))

(defpsmacro prog1 (first &rest others)
  (with-ps-gensyms (val)
    `(let ((,val ,first))
       ,@others
       ,val)))

(defpsmacro prog2 (first second &rest others)
  `(progn ,first (prog1 ,second ,@others)))

(defpsmacro apply (fn &rest args)
  (let ((arglist (if (> (length args) 1)
                     `(append (list ,@(butlast args)) ,(car (last args)))
                     (first args))))
    `(funcall (getprop ,fn 'apply) this ,arglist)))

;;; misc

(defpsmacro defvar (name &optional
                         (value (values) value-provided?)
                         documentation)
  ;; this must be used as a top-level form, otherwise the resulting
  ;; behavior will be undefined.
  (declare (ignore documentation))
  (pushnew name *ps-special-variables*)
  `(var ,name ,@(when value-provided? (list value))))

(defpsmacro let* (bindings &body body)
  (if bindings
      `(let (,(car bindings))
         (let* ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defpsmacro do-set-timeout ((timeout) &body body)
  `(set-timeout (lambda () ,@body) ,timeout))
