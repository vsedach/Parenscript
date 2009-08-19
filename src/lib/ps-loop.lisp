(in-package :parenscript)

(defun complex-js-expr? (expr)
  (if (symbolp expr)
      (or (find #\. (symbol-name expr))
          (not (eq (ps-macroexpand expr) expr)))
      (consp expr)))

(defvar *loop-keywords*
  '(:for :do :when :unless :initially :finally :first-time :last-time :while :until
    :from :to :below :downto :above :by :in :across :on :index := :then :sum :collect
    :count :minimize :maximize :into :repeat))

(defun normalize-loop-keywords (args)
  (mapcar
   (lambda (x)
     (or (find-if (lambda (key) (and (symbolp x) (equal (symbol-name x) (symbol-name key))))
                  *loop-keywords*)
         x))
   args))

(defun reduce-function-symbol (sym)
  (if (and (consp sym) (eq 'function (first sym)))
      (second sym)
      sym))

(defun err (expected got)
  (error "PS-LOOP expected ~a, got ~a." expected got))

(defclass loop-state ()
  ((tokens :initarg :tokens :accessor tokens)
   (iterations :initform nil :accessor iterations)
   (prologue :initform nil :accessor prologue)
   (initially :initform nil :accessor initially)
   (finally :initform nil :accessor finally)
   (during-first :initform nil :accessor during-first)
   (first-guard :initform nil :accessor first-guard)
   (during-last :initform nil :accessor during-last)
   (last-guard :initform nil :accessor last-guard)
   (default-accum-var :initform nil :accessor default-accum-var)
   (default-accum-kind :initform nil :accessor default-accum-kind)
   (body :initform nil :accessor body)))

(defun nreverse-loop-state (state)
  (macrolet ((rev% (&rest accs)
               (cons 'progn (loop :for a :in accs :collect `(setf (,a state) (nreverse (,a state)))))))
    (rev% iterations prologue initially finally during-first during-last body))
  state)

(defun push-tokens (state toks)
  (setf (tokens state) (append toks (tokens state))))

(defun peek (state)
  (car (tokens state)))

(defun eat (state &optional what tag)
  (case what
    (:if (when (eq (peek state) tag)
           (eat state)
           (eat state)))
    (:progn (cons 'progn (loop :collect (if (consp (peek state))
                                            (eat state)
                                            (err "a compound form" (peek state)))
                           :until (atom (peek state)))))
    (otherwise (let ((tok (pop (tokens state))))
                 (when (and (eq what :atom) (not (atom tok)))
                   (err "an atom" tok))
                 tok))))

(defmacro with-local-var ((name expr state) &body body)
  (once-only (expr)
    `(let ((,name (aif (and (complex-js-expr? ,expr) (ps-gensym))
                       (progn (push (list 'var it ,expr) (prologue ,state))
                              it)
                       ,expr)))
       ,@body)))

(defun for-from (var state)
  (let ((start (eat state))
        (op '+)
        (test-op nil)
        (by nil)
        (end nil))
    (loop while (member (peek state) '(:to :below :downto :above :by)) do
          (let ((term (eat state)))
            (if (eq term :by)
                (setf by (eat state))
                (setf op (case term ((:downto :above) '-) (otherwise '+))
                      test-op (case term (:to '>) (:below '>=) (:downto '<) (:above '<=))
                      end (eat state)))))
    (let ((test (when test-op
                  (with-local-var (v end state)
                    (list test-op var v)))))
      (push `(,var nil ,start (,op ,var ,(or by 1)) ,test) (iterations state)))))

(defun for-= (var bindings state)
  (let ((start (eat state))
        (then (eat state :if :then)))
    (push (list var bindings start (or then start) nil) (iterations state))))

(defun for-in (var bindings state)
  (with-local-var (arr (eat state) state)
    (let ((index (or (eat state :if :index) (ps-gensym))))
      (push-tokens state `(,index :from 0 :below (length ,arr)
                                  ,var := (aref ,arr ,index)))
      (for-clause state)
      (for-clause state)
      ;; set bindings associated with original clause, e.g. "loop :for (a b) :in c"
      (setf (second (car (iterations state))) bindings))))

(defun for-on (var bindings state)
  (with-local-var (arr (eat state) state)
    (let ((by (aif (eat state :if :by)
                   `(,(reduce-function-symbol it) ,var)
                   `((@ ,var :slice) 1))))
      (push-tokens state `(,var := ,arr :then ,by))
      (for-clause state)
      (let ((this-iteration (car (iterations state))))
        (setf (second this-iteration) bindings)
        ;; set the end-test
        (setf (fifth this-iteration) `(or (null ,var) (= (length ,var) 0)))))))

(defun for-clause (state)
  (let* ((place (eat state))
         (var (when (atom place) place))
         (bindings (unless var place))
         (term (eat state :atom)))
    (when bindings
      (when (eq term :from)
        (err "an atom after FROM" bindings))
      (setf var (ps-gensym)))
    (case term
      (:from (for-from var state))
      (:= (for-= var bindings state))
      ((:in :across) (for-in var bindings state))
      (:on (for-on var bindings state))
      (otherwise (error "FOR ~s ~s is not valid in PS-LOOP." var term)))))

(defun accumulate (kind term var state)
  (when (null var)
    (when (and (default-accum-kind state) (not (eq kind (default-accum-kind state))))
      (error "PS-LOOP encountered illegal ~a: ~a was already declared, and there can only be one kind of default accumulation per loop." kind (default-accum-kind state)))
    (unless (default-accum-var state)
      (setf (default-accum-var state)
            (ps-gensym (case kind
                         (:minimize 'min)
                         (:maximize 'max)
                         (t kind))))
      (setf (default-accum-kind state) kind))
    (setf var (default-accum-var state)))
  (let ((initial (case kind
                   ((:sum :count) 0)
                   ((:maximize :minimize) nil)
                   (:collect '(array)))))
    (pushnew `(var ,var ,initial) (prologue state) :key #'second))
  (case kind
    (:sum `(incf ,var ,term))
    (:count `(unless (null ,term) (incf ,var)))
    (:minimize `(setf ,var (if (null ,var) ,term (min ,var ,term))))
    (:maximize `(setf ,var (if (null ,var) ,term (max ,var ,term))))
    (:collect `((@ ,var :push) ,term))))

(defun first-time-clause (state)
  (push (eat state :progn) (during-first state))
  (unless (first-guard state)
    (setf (first-guard state) (ps-gensym))))

(defun last-time-clause (state)
  (push (eat state :progn) (during-last state))
  (unless (last-guard state)
    (setf (last-guard state) (ps-gensym))))

(defun repeat-clause (state)
  (let ((index (ps-gensym)))
    (setf (tokens state) (append `(,index :from 0 :below ,(eat state)) (tokens state)))
    (for-clause state)))

(defun body-clause (term state)
  (case term
    ((:when :unless) (list (intern (symbol-name term))
                           (eat state)
                           (body-clause (eat state :atom) state)))
    ((:sum :collect :count :minimize :maximize) (accumulate term (eat state) (eat state :if :into) state))
    (:do (eat state :progn))
    (otherwise (err "a PS-LOOP keyword" term))))

(defun clause (state)
  (let ((term (eat state :atom)))
    (case term
      (:for (for-clause state))
      (:repeat (repeat-clause state))
      (:while (push `(unless ,(eat state) break) (body state)))
      (:until (push `(when ,(eat state) break) (body state)))
      (:initially (push (eat state :progn) (initially state)))
      (:finally (push (eat state :progn) (finally state)))
      (:first-time (first-time-clause state))
      (:last-time (last-time-clause state))
      (otherwise (push (body-clause term state) (body state))))))

(defun parse-ps-loop (terms)
  (if (null terms)
      (err "loop definition" nil)
      (let ((state (make-instance 'loop-state :tokens terms)))
        (loop :while (tokens state) :do (clause state))
        (nreverse-loop-state state))))

(defun multiple-fors? (loop)
  (> (length (iterations loop)) 1))

(defun inits (loop)
  (mapcar (lambda (x) (list (first x) (third x)))
          (iterations loop)))

(defun steps (loop)
  (mapcar (lambda (x) `(setf ,(first x) ,(fourth x)))
          (iterations loop)))

(defun end-test (loop)
  (aif (loop :for (nil nil nil nil test) :in (iterations loop)
         :when test :collect test)
       (if (cdr it)
           (list 'not (cons 'or it))
           (cons 'not it))
       t))

(defun wrap-with-destructurings (iterations forms)
  (if (null iterations)
      forms
      (wrap-with-destructurings
       (cdr iterations)
       (aif (second (car iterations))
            `((destructuring-bind ,it ,(first (car iterations)) ,@forms))
            forms))))

(defun wrap-with-first-and-last-guards (loop forms)
  (append (awhen (during-first loop)
            `((when ,(first-guard loop)
                ,@it
                (setf ,(first-guard loop) nil))))
          forms
          (when (during-last loop)
            `((setf ,(last-guard loop) t)))))

(defun wrap-with-initially-and-finally (loop form)
  `(progn
     ,@(initially loop)
     ,form
     ,@(awhen (during-last loop)
              `((when ,(last-guard loop) ,@it)))
     ,@(finally loop)))

(defun loop-form-with-alternating-tests (loop)
  (let ((form (wrap-with-initially-and-finally
               loop
               `(while t
                  ,@(wrap-with-first-and-last-guards
                     loop
                     (append (body loop)
                             (loop :for (var bindings nil step test) :in (iterations loop)
                               :collect `(setf ,var ,step)
                               :collect `(dset ,bindings ,var)
                               :when test :collect `(when ,test (break)))))))))
    ;; preface the whole thing with alternating inits and tests prior
    ;; to first executing the loop; this way, like CL LOOP, we refrain
    ;; from initializing subsequent clauses if a test fails
    (loop :for (var bindings init nil test) :in (reverse (iterations loop)) :do
      (when test
        (setf form `(unless ,test ,form)))
      (when bindings
        (setf form `(destructuring-bind ,bindings ,var ,form)))
      (setf form `(let ((,var ,init)) ,form)))
    form))

(defun simple-for-form (loop)
  (wrap-with-initially-and-finally
   loop
   `(for ,(inits loop) (,(end-test loop)) ,(steps loop)
         ,@(wrap-with-first-and-last-guards
            loop
            (wrap-with-destructurings (iterations loop) (body loop))))))

(defpsmacro loop (&rest args)
  (let ((loop (parse-ps-loop (normalize-loop-keywords args))))
    `(,@(if (default-accum-var loop) '(with-lambda ()) '(progn))
        ,@(when (during-first loop) `((var ,(first-guard loop) t)))
        ,@(when (during-last loop) `((var ,(last-guard loop) nil)))
        ,@(prologue loop)
        ,(if (multiple-fors? loop)
             (loop-form-with-alternating-tests loop)
             (simple-for-form loop))
        ,@(when (default-accum-var loop) `((return ,(default-accum-var loop)))))))
