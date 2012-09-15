(in-package #:parenscript)

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
   (iterations :initform nil :accessor iterations) ; (place init step test obj)*
   (prologue :initform nil :accessor prologue)
   (finally :initform nil :accessor finally)
   (implicit-accum-var :initform nil :accessor implicit-accum-var)
   (implicit-accum-kind :initform nil :accessor implicit-accum-kind)
   (body :initform nil :accessor body)))

(defun push-tokens (state toks)
  (setf (tokens state) (append toks (tokens state))))

(defun peek (state)
  (car (tokens state)))

(defun eat (state &optional what tag)
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

(defun maybe-extract-var (expr state)
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
                  (list test-op var (maybe-extract-var end state)))))
      (push `(,var ,start (,op ,var ,(or by 1)) ,test) (iterations state)))))

(defun for-= (place state)
  (let ((start (eat state)))
    (multiple-value-bind (then thenp)
        (eat state :if :then)
      (push (list place start (if thenp then start) nil) (iterations state)))))

(defun for-in (place state)
  (let ((arr (maybe-extract-var (eat state) state))
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
    ;; set the end-test
    (setf (fourth (car (iterations state))) `(> (length ,var) 0))
    (unless (eq place var)
      (push-tokens state `(,place := ,var))
      (for-clause state))))

(defun for-keys-of (place state)
  (when (iterations state)
    (error "FOR..OF is only allowed as the first clause in a loop."))
  (when (consp place)
    (unless (<= (length place) 2) ; length 1 is ok, treat (k) as (k nil)
      (error "FOR..OF must be followed by a key variable or key-value pair."))
    (unless (atom (first place))
      (error "The key in a FOR..OF clause must be a variable.")))
  (let ((k (or (if (atom place) place (first place)) (ps-gensym)))
        (v (when (consp place) (second place))))
    (let ((obj (eat state)))
      (when v ; assign OBJ to a local var if we need to for value binding (otherwise inline it)
        (setf obj (maybe-extract-var obj state)))
      (push (list k nil nil nil obj) (iterations state))
      (when v
        (let ((val `(getprop ,obj ,k)))
          (push (list v val val nil) (iterations state)))))))

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
    (when (and (implicit-accum-kind state) (not (eq kind (implicit-accum-kind state))))
      (error "PS-LOOP encountered illegal ~a: ~a was already declared, and there can only be one kind of implicit accumulation per loop." kind (implicit-accum-kind state)))
    (unless (implicit-accum-var state)
      (setf (implicit-accum-var state)
            (ps-gensym (string (loop-case kind
                                     (:minimize 'min)
                                     (:maximize 'max)
                                     (t kind)))))
      (setf (implicit-accum-kind state) kind))
    (setf var (implicit-accum-var state)))
  (let ((initial (loop-case kind
                       ((:sum :count) 0)
                       ((:maximize :minimize) nil)
                       ((:collect :append) '[])
                       ((:map) '{}))))
    (push (list var initial) (prologue state)))
  (loop-case kind
        (:sum `(incf ,var ,item))
        (:count `(unless (null ,item) (incf ,var)))
        (:minimize `(setf ,var (if (null ,var) ,item (min ,var ,item))))
        (:maximize `(setf ,var (if (null ,var) ,item (max ,var ,item))))
        (:collect `((@ ,var :push) ,item))
        (:append `(setf ,var (append ,var ,item)))
        (:map (destructuring-bind (key val) item
                `(setf (getprop ,var ,key) ,val)))))

(defun repeat-clause (state)
  (let ((index (ps-gensym)))
    (setf (tokens state) (append `(,index :from 0 :below ,(eat state)) (tokens state)))
    (for-clause state)))

(defun while-clause (state)
  (push (list nil nil nil (eat state)) (iterations state)))

(defun until-clause (state)
  (push (list nil nil nil `(not ,(eat state))) (iterations state)))

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
          (otherwise (push (body-clause term state) (body state))))))

(defun parse-ps-loop (terms)
  (cond ((null terms) (err "loop definition" nil))
        (t (let ((state (make-instance 'loop-state :tokens terms)))
             (loop :while (tokens state) :do (clause state))
             state))))

(defun fold-tests (iterations)
  ;; unifies adjacent test expressions by destructively modifying iterations
  (let ((folded '()))
    (loop :for iter :in iterations :do
      (let ((place (first iter))
            (test (fourth iter)))
        (cond ((and (null place) (car folded))
               (assert test nil "Iteration ~a has neither PLACE nor TEST." iter)
               (let ((test^ (fourth (car folded))))
                 (setf (fourth (car folded)) (if test^ `(and ,test^ ,test) test))))
              (t (push iter folded)))))
    (nreverse folded)))

(defun augmented-loop-body (body clauses firstvar)
  (cond ((null clauses) body)
        (t (destructuring-bind (place init step test) (car clauses)
             (setf body (augmented-loop-body body (cdr clauses) firstvar))
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
                             (t `((bind ,place ,expr ,@body))))))))
           body)))

(defun master-loop (master body)
  (destructuring-bind (place init step test &optional obj) master
    (cond ((null place) `(while ,test ,@body))
          (obj `(for-in (,place ,obj) ,@body))
          (t (assert (atom place) nil "Unexpected destructuring list ~a in master loop" place)
             `(for ((,place ,init)) (,(or test t)) ((setf ,place ,step)) ,@body)))))

(defun build-loop (iterations body)
  (destructuring-bind (master . clauses) iterations
    (let ((firstvar (loop :for (nil init step) :in clauses
                      :unless (tree-equal init step) :do
                      (return (ps-gensym "first")))))
      (setf body (augmented-loop-body body clauses firstvar))
      (when firstvar
        (setf body (append body `((setf ,firstvar nil)))))
      (let ((form (master-loop master body)))
        (if firstvar `(let ((,firstvar t)) ,form) form)))))

(defun normalize (iterations)
  (cond ((null iterations) (list (list nil nil nil t))) ; while (true)
        (t (destructuring-bind (master . clauses) (fold-tests iterations)
             (destructuring-bind (place init step test &optional obj) master
               (when (and (null obj) (complex-js-expr? place))
                 (let ((var (ps-gensym)))
                   (setf master (list var init step test))
                   (push (list place var var nil) clauses)))
               (cons master clauses))))))

(defun prologue-wrap (prologue body)
  (cond ((null prologue) body)
        (t (destructuring-bind (place expr) (car prologue)
             (prologue-wrap
              (cdr prologue)
              (cond ((atom place) (cons `(var ,place ,expr) body))
                    (t `((bind ,place ,expr ,@body)))))))))

(defpsmacro loop (&rest keywords-and-forms)
  (let* ((state (parse-ps-loop keywords-and-forms))
         (main `(,(build-loop (normalize (reverse (iterations state))) (reverse (body state)))
                  ,@(reverse (finally state))
                  ,@(awhen (implicit-accum-var state) (list it))))
         (full `(block ,(name state) ,@(prologue-wrap (prologue state) main))))
    (if (implicit-accum-var state)
        `((lambda () ,full))
        full)))
