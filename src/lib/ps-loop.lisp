(in-package #:parenscript)

;;; bind and bind* - macros used for destructuring bindings in PS LOOP

(defun dot->rest (x)
  (cond ((atom x) x)
        ((not (listp (cdr x)))        ; dotted list
         (list (dot->rest (car x)) '&rest (dot->rest (cdr x))))
        (t (cons (dot->rest (car x)) (dot->rest (cdr x))))))

(defun property-bindings-p (x)
  (when (consp x)
    (every (lambda (y)
             (or (keywordp y) ; standalone property name
                 (and (consp y) ; var name paired with property name
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
              :collect d :into ds
              :when p :append p :into ps
              :finally (return (list ds ps))))))

(defun property-bindings (bindings expr body)
  `(let ,(loop :for b :in bindings
            :for (var p) = (cond ((consp b) b) ; var name paired with property name
                                 (t (list (intern (string b)) b))) ; make var from prop
            :collect `(,var (@ ,expr ,p)))
     ,@body))

(defpsmacro bind (bindings expr &body body)
  (let ((bindings (dot->rest bindings)))
    (destructuring-bind (d p)
        (extract-bindings bindings)
      (cond ((and (atom d)
                  (or (= (length bindings) 1)
                      (atom (ps-macroexpand expr))))
             (property-bindings bindings expr body))
            ((atom d)
             (with-ps-gensyms (var)
               `(let ((,var ,expr))
                  (bind ,bindings ,var ,@body))))
            ((null p)
             `(destructuring-bind ,bindings ,expr ,@body))
            (t `(destructuring-bind ,d ,expr
                  (bind* ,p ,@body)))))))

(defpsmacro bind* (bindings &body body)
  (cond ((= (length bindings) 2)
         `(bind ,(car bindings) ,(cadr bindings) ,@body))
        (t `(bind ,(car bindings) ,(cadr bindings)
              (bind* ,(cddr bindings) ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *loop-keywords*
    '(:named :for :repeat :with :while :until :initially :finally
      :from :to :below :downto :above :by :in :across :on := :then
      :when :unless :if :else :end :do :return
      :sum :collect :append :count :minimize :maximize :map :of :into))

  (defun as-keyword (key)
    (cond ((not (symbolp key)) key)
          ((keywordp key) key)
          (t (intern (symbol-name key) :keyword)))))

(defmacro loop-case (key &body forms)
  (loop :for (match . nil) :in forms
    :for keys = (if (listp match) match (list match)) :do
    (loop :for k :in keys :do
      (assert (member k (append *loop-keywords* '(t otherwise)))
              nil "~a isn't a recognized loop keyword." k)))
  `(case (as-keyword ,key) ,@forms))

(defun err (expected got)
  (error "PS-LOOP expected ~a, got ~a." expected got))

(defclass loop-state ()
  ((tokens :initarg :tokens :accessor tokens)
   (name :initform nil :accessor name)
   ;; A clause is either (:BODY FORM) or (:ITER PLACE INIT STEP TEST &OPTIONAL JS-OBJ)
   (clauses :initform nil :accessor clauses)
   (prologue :initform nil :accessor prologue)
   (finally :initform nil :accessor finally)
   (accum-var :initform nil :accessor accum-var)
   (accum-kind :initform nil :accessor accum-kind)))

(defun push-body-clause (clause state)
  (push (list :body clause) (clauses state)))

(defun push-iter-clause (clause state)
  (push (cons :iter clause) (clauses state)))

(defun push-tokens (state toks)
  (setf (tokens state) (append toks (tokens state))))

(defun peek (state)
  (car (tokens state)))

(defun eat (state &optional what tag)
  "Consumes the next meaningful chunk of loop for processing."
  (case what
    (:if (when (eq (as-keyword (peek state)) tag)
           (eat state)
           (values (eat state) t)))
    (:progn (cons 'progn (loop :collect (if (consp (peek state))
                                            (eat state)
                                            (err "a compound form" (peek state)))
                           :until (atom (peek state)))))
    (otherwise (let ((tok (pop (tokens state))))
                 (when (and (eq what :atom) (not (atom tok)))
                   (err "an atom" tok))
                 (when (and (eq what :symbol) (not (symbolp tok)))
                   (err "a symbol" tok))
                 tok))))

(defun maybe-hoist (expr state)
  (cond ((complex-js-expr? expr)
         (let ((var (ps-gensym)))
           (push (list var expr) (prologue state))
           var))
        (t expr)))

(defun for-from (var state)
  (unless (atom var)
    (err "an atom after FROM" var))
  (let ((start (eat state))
        (op '+)
        (test-op nil)
        (by nil)
        (end nil))
    (loop while (member (as-keyword (peek state)) '(:to :below :downto :above :by)) do
          (let ((term (eat state)))
            (if (eq (as-keyword term) :by)
                (setf by (eat state))
                (setf op (loop-case term ((:downto :above) '-) (otherwise '+))
                      test-op (loop-case term (:to '<=) (:below '<) (:downto '>=) (:above '>))
                      end (eat state)))))
    (let ((test (when test-op
                  (list test-op var (maybe-hoist end state)))))
      (push-iter-clause `(,var ,start (,op ,var ,(or by 1)) ,test) state))))

(defun for-= (place state)
  (let ((start (eat state)))
    (multiple-value-bind (then thenp)
        (eat state :if :then)
      (push-iter-clause (list place start (if thenp then start) nil) state))))

(defun for-in (place state)
  (let ((arr (maybe-hoist (eat state) state))
        (index (ps-gensym)))
    (push-tokens state `(,index :from 0 :below (length ,arr)
                                ,place := (aref ,arr ,index)))
    (for-clause state)
    (for-clause state)))

(defun for-on (place state)
  (let* ((arr (eat state))
         (by (or (eat state :if :by) 1))
         (var (if (atom place) place (ps-gensym)))
         (then (if (numberp by) `((@ ,var :slice) ,by) `(,by ,var))))
    (push-tokens state `(,var := ,arr :then ,then))
    (for-clause state)
    ;; set the end-test by snooping into the iteration clause we just added
    (setf (fifth (car (clauses state))) `(> (length ,var) 0))
    (unless (eq place var)
      (push-tokens state `(,place := ,var))
      (for-clause state))))

(defun for-keys-of (place state)
  (when (clauses state)
    (error "FOR..OF is only allowed as the first clause in a loop."))
  (when (consp place)
    (unless (<= (length place) 2) ; length 1 is ok, treat (k) as (k nil)
      (error "FOR..OF must be followed by a key variable or key-value pair."))
    (unless (atom (first place))
      (error "The key in a FOR..OF clause must be a variable.")))
  (let ((k (or (if (atom place) place (first place)) (ps-gensym)))
        (v (when (consp place) (second place))))
    (let ((js-obj (eat state)))
      (when v ; assign JS-OBJ to a local var if we need to for value binding (otherwise inline it)
        (setf js-obj (maybe-hoist js-obj state)))
      (push-iter-clause (list k nil nil nil js-obj) state)
      (when v
        (let ((val `(getprop ,js-obj ,k)))
          (push-iter-clause (list v val val nil) state))))))

(defun for-clause (state)
  (let ((place (eat state))
        (term (eat state :atom)))
    (loop-case term
          (:from (for-from place state))
          (:= (for-= place state))
          ((:in :across) (for-in place state))
          (:on (for-on place state))
          (:of (for-keys-of place state))
          (otherwise (error "FOR ~s ~s is not valid in PS-LOOP." place term)))))

(defun a-with-clause (state) ;; so named to avoid with-xxx macro convention
  (let ((place (eat state)))
    (push (list place (eat state :if :=)) (prologue state))))

(defun accumulate (kind item var state)
  (when (null var)
    (when (and (accum-kind state) (not (eq kind (accum-kind state))))
      (error "PS-LOOP encountered illegal ~a: ~a was already declared, and there can only be one kind of implicit accumulation per loop." kind (accum-kind state)))
    (unless (accum-var state)
      (setf (accum-var state)
            (ps-gensym (string (loop-case kind
                                     (:minimize 'min)
                                     (:maximize 'max)
                                     (t kind)))))
      (setf (accum-kind state) kind))
    (setf var (accum-var state)))
  (let ((initial (loop-case kind
                       ((:sum :count) 0)
                       ((:maximize :minimize) nil)
                       ((:collect :append) '[])
                       ((:map) '{}))))
    (push (list var initial) (prologue state)))
  (loop-case kind
        (:sum `(incf ,var ,item))
        (:count `(when ,item (incf ,var))) ;; note the JS semantics - neither 0 nor "" will count
        (:minimize `(setf ,var (if (null ,var) ,item (min ,var ,item))))
        (:maximize `(setf ,var (if (null ,var) ,item (max ,var ,item))))
        (:collect `((@ ,var 'push) ,item))
        (:append `(setf ,var (append ,var ,item)))
        (:map (destructuring-bind (key val) item
                `(setf (getprop ,var ,key) ,val)))))

(defun repeat-clause (state)
  (let ((index (ps-gensym)))
    (setf (tokens state) (append `(,index :from 0 :below ,(eat state)) (tokens state)))
    (for-clause state)))

(defun while-clause (state)
  (push-iter-clause (list nil nil nil (eat state)) state))

(defun until-clause (state)
  (push-iter-clause (list nil nil nil `(not ,(eat state))) state))

(defun body-clause (term state)
  (loop-case term
        ((:if :when :unless)
         (let* ((test-form (eat state))
                (seqs (list (body-clause (eat state :atom) state)))
                (alts (list)))
           (loop while (eq (as-keyword (peek state)) :and)
                 do (eat state)
                    (push (body-clause (eat state :atom) state) seqs))
           (when (eq (as-keyword (peek state)) :else)
             (eat state)
             (push (body-clause (eat state :atom) state) alts)
             (loop while (eq (as-keyword (peek state)) :and)
                   do (eat state)
                      (push (body-clause (eat state :atom) state) alts)))
           (when (eq (as-keyword (peek state)) :end)
             (eat state))
           (if (null alts)
               `(,(loop-case term ((:unless) 'unless) (otherwise 'when))
                 ,test-form
                 ,@(reverse seqs))
               `(if ,(loop-case term
                       ((:unless) `(not ,test-form))
                       (otherwise test-form))
                    (progn ,@(reverse seqs))
                    (progn ,@(reverse alts))))))
        ((:sum :collect :append :count :minimize :maximize)
         (accumulate term (eat state) (eat state :if :into) state))
        (:map (let ((key (eat state)))
                (multiple-value-bind (val valp)
                    (eat state :if :to)
                  (unless valp
                    (error "MAP must be followed by a TO to specify value."))
                  (accumulate :map (list key val) (eat state :if :into) state))))
        (:do (eat state :progn))
        (:return `(return-from ,(name state) ,(eat state)))
        (otherwise (err "a PS-LOOP keyword" term))))

(defun clause (state)
  (let ((term (eat state :atom)))
    (loop-case term
          (:named (setf (name state) (eat state :symbol)))
          (:with (a-with-clause state))
          (:for (for-clause state))
          (:repeat (repeat-clause state))
          (:while (while-clause state))
          (:until (until-clause state))
          (:finally (push (eat state :progn) (finally state)))
          (otherwise (push-body-clause (body-clause term state) state)))))

(defun parse-ps-loop (terms)
  (cond ((null terms) (err "loop definition" nil))
        (t (let ((state (make-instance 'loop-state :tokens terms)))
             (loop :while (tokens state) :do (clause state))
             state))))

(defun fold-iterations-where-possible (clauses)
  (let ((folded '()))
    (loop :for clause :in clauses :do
      (assert (member (car clause) '(:iter :body)))
      (let ((folded? nil))
        (when (and (eq (car clause) :iter) (eq (caar folded) :iter))
          (destructuring-bind (tag place init step test &optional js-obj) clause
            (declare (ignore tag))
            (when (null place) ;; can't combine two iterations that both have state
              (assert (not (or init step js-obj)) nil "Invalid iteration ~a: PLACE should not be null." clause)
              (assert test nil "Iteration ~a has neither PLACE nor TEST." clause)
              (unless (sixth (car folded)) ;; js-obj means a for..in loop and those can't have tests
                (let ((prev-test (fifth (car folded))))
                  (setf (fifth (car folded)) (if prev-test `(and ,prev-test ,test) test))
                  (setf folded? t))))))
        (unless folded?
          (push clause folded))))
    (nreverse folded)))

(defun organize-iterations (clauses)
  ;; we want clauses to start with a master loop to provide the
  ;; skeleton for everything else. secondary iterations are ok but
  ;; will be generated inside the body of this master loop
  (unless (eq (caar clauses) :iter)
    (push (list :iter nil nil nil t) clauses))
  ;; unify adjacent test expressions by ANDing them together where possible
  (setf clauses (fold-iterations-where-possible clauses))
  ;; if leading iteration has a binding expression, replace it with a var
  (destructuring-bind (tag place init step test &optional js-obj) (car clauses)
    (assert (eq tag :iter))
    (when (complex-js-expr? place)
      (assert (null js-obj) nil "Invalid iteration ~a: FOR..IN can't have a binding expression." (car clauses))
      (let ((var (ps-gensym)))
        (pop clauses)
        (push (list :iter place var var nil) clauses)
        (push (list :iter var init step test) clauses))))
  clauses)

(defun build-body (clauses firstvar)
  (cond ((null clauses) nil)
        ((eq (caar clauses) :body)
         (cons (second (car clauses)) (build-body (cdr clauses) firstvar)))
        (t (destructuring-bind (tag place init step test) (car clauses)
             (assert (eq tag :iter))
             (let ((body (build-body (cdr clauses) firstvar)))
               (when test
                 (push `(unless ,test (break)) body))
               (when place
                 (let ((expr (if (tree-equal init step) init `(if ,firstvar ,init ,step))))
                   (setf body
                         (cond ((and (atom place) (eq expr init))
                                `((let ((,place ,expr)) ,@body)))
                               ;; can't use LET because EXPR may reference PLACE
                               ((atom place) `((var ,place ,expr) ,@body))
                               ;; BIND has scoping problems. For example,
                               ;; (loop :for (a b) = x :then b) doesn't work
                               ;; since EXPR is referencing part of PLACE.
                               ;; But the following is ok for known uses so far.
                               (t `((bind ,place ,expr ,@body)))))))
               body)))))

(defun master-loop (master-iter body)
  (destructuring-bind (tag place init step test &optional js-obj) master-iter
    (assert (eq tag :iter))
    (cond ((null place) `(while ,test ,@body))
          (js-obj
           (assert (not (or init step test)) nil "Unexpected iteration state in for..in loop: ~a" master-iter)
           `(for-in (,place ,js-obj) ,@body))
          (t (assert (atom place) nil "Unexpected destructuring list ~a in master loop" place)
             `(for ((,place ,init)) (,(or test t)) ((setf ,place ,step)) ,@body)))))

(defun build-loop (clauses)
  (destructuring-bind (master . rest) clauses
    (assert (eq (car master) :iter) nil "First clause is not master loop: ~a" master)
    (let* ((firstvar (loop :for (tag nil init step) :in rest
                       :when (and (eq tag :iter) (not (tree-equal init step)))
                       :do (return (ps-gensym "first"))))
           (body (build-body rest firstvar)))
      (when firstvar
        (setf body (append body `((setf ,firstvar nil)))))
      (let ((form (master-loop master body)))
        (if firstvar `(let ((,firstvar t)) ,form) form)))))

(defun prologue-wrap (prologue body)
  (cond ((null prologue) body)
        (t (destructuring-bind (place expr) (car prologue)
             (prologue-wrap
              (cdr prologue)
              (cond ((atom place) (cons `(var ,place ,expr) body))
                    (t `((bind ,place ,expr ,@body)))))))))

(defpsmacro loop (&rest keywords-and-forms)
  (let ((state (parse-ps-loop keywords-and-forms)))
    (let* ((clauses (organize-iterations (reverse (clauses state))))
           (main `(,(build-loop (organize-iterations clauses))
                    ,@(reverse (finally state))
                    ,@(awhen (accum-var state) (list it))))
           (full `(block ,(name state) ,@(prologue-wrap (prologue state) main))))
      (if (accum-var state)
          (lambda-wrap full)
          full))))
