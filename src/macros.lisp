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

(defpsmacro multiple-value-bind (vars form &body body)
  (let* ((form (ps-macroexpand form))
         (progn-form
          (when (and (consp form)
                     (member
                      (car form)
                      '(with label let flet labels macrolet symbol-macrolet progn)))
            (pop form))))
    (with-ps-gensyms (mv prev-mv)
      `(let (,prev-mv)
         (,(or progn-form 'progn)
          ,@(when progn-form (butlast form))
          (setf ,prev-mv (@ arguments :callee :mv))
          (try
           (progn
             (setf (@ arguments :callee :mv) t)
             (let ((,(car vars) ,(if progn-form (car (last form)) form))
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
             `(defun-setf ,name ,lambda-list ,@body))))

;;; defining setf expanders

(defvar *defun-setf-name-prefix* '__setf_)

(defpsmacro defun-setf (setf-name lambda-list &body body)
  (let ((mangled-function-name
         (intern (concatenate 'string (string *defun-setf-name-prefix*) (string (second setf-name)))
                 (symbol-package (second setf-name)))))
    (setf (gethash (second setf-name) *setf-expanders*)
          (compile
           nil
           (lambda (access-args store-form)
             `(,mangled-function-name ,store-form ,@access-args))))
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
  (if result?
      `((lambda ()
          (for ,(do-make-for-vars/init decls) ((not ,termination)) ,(do-make-for-steps decls)
               ,@body)
          ,result))
      `(for ,(do-make-for-vars/init decls) ((not ,termination)) ,(do-make-for-steps decls)
            ,@body)))

(defpsmacro do (decls (termination &optional (result nil result?)) &body body)
  (if result?
      `((lambda ,(do-make-init-vars decls)
          (for () ((not ,termination)) ()
               ,@body
               ,(do-make-iter-psteps decls))
          ,result)
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

(defun destructuring-wrap (arr n bindings body &key setf?)
  (labels ((bind-expr (var expr inner-body)
             (if setf?
                 `(progn (setf ,var ,expr) ,inner-body)
                 `(let ((,var ,expr)) ,inner-body)))
           (bind-rest (sym)
             (bind-expr sym `(if (> (length ,arr) ,n)
                                 ((@ ,arr slice) ,n)
                                 '())
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

(eval-when (:compile-toplevel :load-toplevel :execute)
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
           (let ((var (gensym)))
             (list var (list x var))))
          (t (loop :for y :on x
               :for (d p) = (extract-bindings (car y))
               :append (cond ((listp (cdr y)) (list d))
                             (t (cons d (cdr y)))) ; dotted list
               :into ds
               :when p :append p :into ps
               :finally (return (list ds ps))))))

  (defun property-bindings (bindings expr body &key setf?)
    (let ((bind-exprs
           (loop :for b :in bindings
             :for (var p) = (if (consp b) b (list (intern (string b)) b))
             :if setf? :collect `(setf ,var (@ ,expr ,p))
             :else :collect `(,var (@ ,expr ,p)))))
      (if setf?
          `(progn ,@bind-exprs ,@body)
          `(let (,@bind-exprs) ,@body)))))

(defpsmacro bind (bindings expr &body body)
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

(defpsmacro bset (bindings expr &body body)
  (destructuring-bind (d p)
      (extract-bindings bindings)
    (cond ((and (atom d)
                (or (= (length bindings) 1)
                    (atom expr)
                    (atom (ps-macroexpand expr))))
           (property-bindings bindings expr body :setf? t))
          ((atom d)
           (with-ps-gensyms (var)
             `(let ((,var ,expr))
                (bind ,bindings ,var ,@body))))
          ((null p) `(dset ,bindings ,expr ,@body))
          (t `(dset ,d ,expr (bset* ,p ,@body))))))

(defpsmacro bset* (bindings &body body)
  (cond ((= (length bindings) 2)
         `(bset ,(car bindings) ,(cadr bindings) ,@body))
        (t `(bset ,(car bindings) ,(cadr bindings)
                  (bset* ,(cddr bindings) ,@body)))))

;;; Control structures

(defpsmacro return (&optional result)
  `(return-from nil ,result))

(defpsmacro ignore-errors (&body forms)
  (with-ps-gensyms (e)
    `(try (progn ,@forms)
          (:catch (,e) nil))))

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
