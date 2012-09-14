(in-package #:parenscript)
(in-readtable :parenscript)

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
    (sinh (n) `((lambda (x) (/ (- (exp x) (exp (- x))) 2)) ,n))
    (cosh (n) `((lambda (x) (/ (+ (exp x) (exp (- x))) 2)) ,n))
    (tanh (n) `((lambda (x) (/ (- (exp x) (exp (- x))) (+ (exp x) (exp (- x))))) ,n))
    (asinh (n) `((lambda (x) (log (+ x (sqrt (1+ (* x x)))))) ,n))
    (acosh (n) `((lambda (x) (* 2 (log (+ (sqrt (/ (1+ x) 2)) (sqrt (/ (1- x) 2)))))) ,n))
    (atanh (n) `((lambda (x) (/ (- (log (+ 1 x)) (log (- 1 x))) 2)) ,n))
    (1+ (n) `(+ ,n 1))
    (1- (n) `(- ,n 1))
    (abs (n) `((@ *math abs) ,n))
    (evenp (n) `(not (oddp ,n)))
    (oddp (n) `(rem ,n 2))
    (exp (n) `((@ *math exp) ,n))
    (expt (base power) `((@ *math pow) ,base ,power))
    (log (n &optional base)
      (or (and (null base) `((@ *math log) ,n))
          (and (numberp base) (= base 10) `(* (log ,n) (@ *math *log10e*)))
          `(/ (log ,n) (log ,base))))
    (sqrt (n) `((@ *math sqrt) ,n))
    (random (&optional upto) (if upto
                                 `(floor (* ,upto (random)))
                                 '(funcall (@ *math random)))))

(defpsmacro ash (integer count)
  (let ((count (ps-macroexpand count)))
    (cond ((and (numberp count) (> count 0)) `(<< ,integer ,count))
          ((numberp count) `(>> ,integer ,(- count)))
          ((complex-js-expr? count)
           (let ((count-var (ps-gensym)))
             `(let ((,count-var ,count))
                (if (> ,count-var 0)
                    (<< ,integer ,count-var)
                    (>> ,integer (- ,count-var))))))
          (t `(if (> ,count 0)
                  (<< ,integer ,count)
                  (>> ,integer (- ,count)))))))

(define-ps-symbol-macro pi (getprop *math '*pi*))

;;; Types

(defpsmacro stringp (x)
  `(string= (typeof ,x) "string"))

(defpsmacro numberp (x)
  `(string= (typeof ,x) "number"))

(defpsmacro functionp (x)
  `(string= (typeof ,x) "function"))

(defpsmacro booleanp (x)
  `(string= (typeof ,x) "boolean"))

;;; Data structures

(defpsmacro make-array (&rest initial-values)
  `(new (*array ,@initial-values)))

(defpsmacro length (a)
  `(getprop ,a 'length))

;;; Getters

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

(defpsmacro multiple-value-bind (vars form &body body)
  (let* ((form (ps-macroexpand form))
         (progn-form
          (when (and (consp form)
                     (member
                      (car form)
                      '(with label let flet labels macrolet symbol-macrolet progn)))
            (pop form))))
    (if progn-form
        `(,progn-form
           ,@(butlast form)
           (multiple-value-bind ,vars
               ,@(last form)
             ,@body))
        ;; assume function call
        (with-ps-gensyms (prev-mv mv)
          (let* ((fun-exp (car form))
                 (funobj (if (symbolp fun-exp)
                             fun-exp
                             (ps-gensym "funobj"))))
           `(let (,@(unless (symbolp fun-exp) `((,funobj ,fun-exp)))
                  (,prev-mv (if (undefined __PS_MV_REG)
                                (setf __PS_MV_REG undefined)
                                __PS_MV_REG)))
              (try
               (let ((,(car vars) (,funobj ,@(cdr form))))
                 (destructuring-bind (&optional ,@(cdr vars))
                     (if (eql ,funobj (@ __PS_MV_REG :tag))
                         (@ __PS_MV_REG :values)
                         (list))
                   ,@body))
               (:finally (setf __PS_MV_REG ,prev-mv)))))))))

;;; conditionals

(defpsmacro case (value &rest clauses)
  (let ((allowed-symbols '(t otherwise false %true)))
   (labels ((make-switch-clause (val body more)
              (cond ((listp val)
                     (append (mapcar #'list (butlast val))
                             (make-switch-clause
                              (if (eq t (car (last val))) ;; literal 'true'
                                  '%true
                                  (car (last val)))
                              body
                              more)))
                    ((and (symbolp val)
                          (symbolp (ps-macroexpand-1 val))
                          (not (keywordp val))
                          (not (member val allowed-symbols)))
                     (error "Parenscript only supports keywords, numbers, and string literals as keys in case clauses. ~S is a symbol in clauses ~S"
                            val clauses))
                    (t
                     `((,(case val
                           ((t otherwise) 'default)
                           (%true          t)
                           (t              (ps-macroexpand-1 val)))
                         ,@body
                         ,@(when more '(break))))))))
     `(switch ,value ,@(mapcon (lambda (clause)
                                 (make-switch-clause (car (first clause))
                                                     (cdr (first clause))
                                                     (rest clause)))
                               clauses)))))

(defpsmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(defpsmacro unless (test &rest body)
  `(when (not ,test) ,@body))

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
      (progn (setf (gethash name *function-lambda-list*) lambda-list)
             `(defun% ,name ,lambda-list ,@body))
      (progn (assert (and (listp name) (= (length name) 2) (eq 'setf (car name))) ()
                     "(defun ~s ~s ...) needs to have a symbol or (setf symbol) for a name." name lambda-list)
             `(defun-setf ,(second name) ,lambda-list ,@body))))

;;; defining setf expanders

(defvar *defun-setf-name-prefix* '__setf_)

(defpsmacro defun-setf (name lambda-list &body body)
  (let ((mangled-function-name
         (intern (format nil "~A~A" (string *defun-setf-name-prefix*) (string name))
                 (symbol-package name))))
    (setf (gethash name *setf-expanders*)
          (lambda (access-args store-form)
            `(,mangled-function-name ,store-form ,@access-args)))
    `(defun ,mangled-function-name ,lambda-list ,@body)))

;;; slightly broken WRT lambda lists
(defpsmacro defsetf-long (access-fn lambda-list (store-var) form)
  (setf (gethash access-fn *setf-expanders*)
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
  (setf (gethash access-fn *setf-expanders*)
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
                 (aif (and (listp place) (gethash (car place) *setf-expanders*))
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
  `(block nil
     (for ,(do-make-for-vars/init decls)
          ((not ,termination))
          ,(do-make-for-steps decls)
          ,@body)
     ,@(when result? (list result))))

(defpsmacro do (decls (termination &optional (result nil result?)) &body body)
  `(block nil
     (let ,(do-make-let-bindings decls)
       (for () ((not ,termination)) ()
            ,@body
            ,(do-make-iter-psteps decls))
       ,@(when result? (list result)))))

(defpsmacro dotimes ((var count &optional (result nil result?)) &rest body)
  `(do* ((,var 0 (1+ ,var)))
        ((>= ,var ,count) ,@(when result? (list result)))
     ,@body))

(defpsmacro dolist ((var array &optional (result nil result?)) &body body)
  (let* ((idx (ps-gensym "_JS_IDX"))
         (introduce-array-var? (not (symbolp array)))
         (arrvar (if introduce-array-var?
                     (ps-gensym "_JS_ARRVAR")
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

(defpsmacro append (arr1 &rest arrs)
  (if arrs
      `((@ ,arr1 concat) ,@arrs)
      arr1))

;;; Destructuring bind

(defun complex-js-expr? (expr)
  (consp (if (symbolp expr) (ps-macroexpand expr) expr)))

(defun hoist-expr? (bindings expr)
  (and (> (length bindings) 1) (complex-js-expr? expr)))

(defun destructuring-wrap (arr n bindings body)
  (cond ((null bindings) body)
        ((eq (car bindings) '&rest)
         (cond ((and (= (length bindings) 2) (atom (second bindings)))
                `(let ((,(second bindings) (if (> (length ,arr) ,n) ((@ ,arr slice) ,n) '())))
                   ,body))
               (t (error "~a is invalid in destructuring list." bindings))))
        ((eq (car bindings) '&optional)
         (destructuring-wrap arr n (cdr bindings) body))
        (t (let ((var (car bindings))
                 (inner-body (destructuring-wrap arr (1+ n) (cdr bindings) body)))
             (cond ((null var) inner-body)
                   ((atom var) `(let ((,var (aref ,arr ,n))) ,inner-body))
                   (t `(,'destructuring-bind ,var (aref ,arr ,n) ,inner-body)))))))

(defpsmacro destructuring-bind (bindings expr &body body)
  (setf bindings (dot->rest bindings))
  (let* ((arr (if (hoist-expr? bindings expr) (ps-gensym "_DB") expr))
         (bound (destructuring-wrap arr 0 bindings (cons 'progn body))))
    (cond ((eq arr expr) bound)
          (t `(let ((,arr ,expr)) ,bound)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

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
       ,@body)))

(defpsmacro bind (bindings expr &body body)
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
  (cond ((= (length bindings) 2)
         `(bind ,(car bindings) ,(cadr bindings) ,@body))
        (t `(bind ,(car bindings) ,(cadr bindings)
              (bind* ,(cddr bindings) ,@body)))))

;;; Control structures

(defpsmacro return (&optional result)
  `(return-from nil ,result))

(defpsmacro ignore-errors (&body forms)
  (with-ps-gensyms (e)
    `(try (progn ,@forms)
          (:catch (,e) nil))))

(defpsmacro unwind-protect (protected-form cleanup-form)
  `(try ,protected-form
        (:finally ,cleanup-form)))

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

(defpsmacro let* (bindings &body body)
  (if bindings
      `(let (,(car bindings))
         (let* ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defpsmacro in-package (package-designator)
  `(eval-when (:compile-toplevel)
     (in-package ,package-designator)))

(defpsmacro use-package (package-designator &optional package)
  `(eval-when (:compile-toplevel)
     (use-package ,package-designator ,@(when package (list package)))))
