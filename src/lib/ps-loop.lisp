(in-package :parenscript)

(defun complex-js-expr? (expr)
  (if (symbolp expr)
      (or (find #\. (symbol-name expr))
          (not (eq (ps-macroexpand expr) expr)))
      (consp expr)))

(defvar *loop-keywords*
  '(:for :do :when :unless :initially :finally :first-time :last-time :while :until
    :from :to :below :downto :above :by :in :across :on :index := :then :sum :collect
    :count :minimize :maximize :into))

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

(defun parse-ps-loop (terms)
  (let (prologue
        init-step-forms end-test-forms
        initially finally
        first-time last-time
        default-accum-var default-accum-kind
        destructurings body)
    (macrolet ((with-local-var ((name expr) &body body)
                 (once-only (expr)
                   `(let ((,name (aif (and (complex-js-expr? ,expr) (ps-gensym))
                                      (progn (push (list 'var it ,expr) prologue)
                                             it)
                                      ,expr)))
                      ,@body))))
      (labels ((next ()
                 (car terms))
               (next? (term)
                 (eq (next) term))
               (err (expected got)
                 (error "PS-LOOP expected ~a, got ~a." expected got))
               (consume (&optional what)
                 (let ((term (pop terms)))
                   (when (and what (not (eq what term)))
                     (err what term))
                   term))
               (consume-atom ()
                 (if (atom (next))
                     (consume)
                     (err "an atom" (next))))
               (consume-progn ()
                 (cons 'progn (loop :collect (if (consp (next))
                                                 (consume)
                                                 (err "a compound form" (next)))
                                :until (atom (next)))))
               (consume-if (term)
                 (when (next? term)
                   (consume)
                   (consume)))
               (accumulate (kind term var)
                 (when (null var)
                   (when (and default-accum-kind (not (eq kind default-accum-kind)))
                     (error "PS-LOOP encountered illegal ~a: ~a was already declared, and there can only be one kind of default accumulation per loop." kind default-accum-kind))
                   (unless default-accum-var
                     (setf default-accum-var (ps-gensym (case kind
                                                          (:minimize 'min)
                                                          (:maximize 'max)
                                                          (t kind)))
                           default-accum-kind kind))
                   (setf var default-accum-var))
                 (let ((initial (case kind
                                  ((:sum :count) 0)
                                  ((:maximize :minimize) nil)
                                  (:collect '(array)))))
                   (pushnew `(var ,var ,initial) prologue :key #'second))
                 (case kind
                   (:sum `(incf ,var ,term))
                   (:count `(incf ,var))
                   (:minimize `(setf ,var (if (null ,var) ,term (min ,var ,term))))
                   (:maximize `(setf ,var (if (null ,var) ,term (max ,var ,term))))
                   (:collect `((@ ,var :push) ,term))))
               (body-clause (term)
                 (case term
                   ((:when :unless) (list (intern (symbol-name term))
                                          (consume)
                                          (body-clause (consume-atom))))
                   ((:sum :collect :count :minimize :maximize) (accumulate term (consume) (consume-if :into)))
                   (:do (consume-progn))
                   (otherwise (err "a PS-LOOP keyword" term))))
               (for-from (var)
                 (let ((start (consume))
                       (op '+)
                       (test nil)
                       (by nil)
                       (end nil))
                   (loop while (member (next) '(:to :below :downto :above :by)) do
                         (let ((term (consume)))
                           (if (eq term :by)
                               (setf by (consume))
                               (setf op (case term ((:downto :above) '-) (otherwise '+))
                                     test (case term (:to '>) (:below '>=) (:downto '<) (:above '<=))
                                     end (consume)))))
                   (push `(,var ,start (,op ,var ,(or by 1))) init-step-forms)
                   (when test
                     (with-local-var (end-var end)
                       (push (list test var end-var) end-test-forms)))))
               (for-= (var)
                 (let ((start (consume))
                       (then (consume-if :then)))
                   (push (list var start (or then start)) init-step-forms)))
               (for-in (var)
                 (with-local-var (arr (consume))
                   (let* ((index (or (consume-if :index) (ps-gensym)))
                          (equiv `(:for ,index :from 0 :below (length ,arr)
                                        :for ,var := (aref ,arr ,index))))
                     (setf terms (append equiv terms))
                     (clause)
                     (clause))))
               (for-on (var)
                 (with-local-var (arr (consume))
                   (push `(or (null ,var) (= (length ,var) 0)) end-test-forms)
                   (let* ((by (aif (consume-if :by)
                                   `(,(reduce-function-symbol it) ,var)
                                   `((@ ,var :slice) 1)))
                          (equiv `(:for ,var := ,arr :then ,by)))
                     (setf terms (append equiv terms))
                     (clause))))
               (for-clause ()
                 (let* ((place (consume))
                        (var (when (atom place) place))
                        (varlist (unless var place))
                        (term (consume-atom)))
                   (when varlist
                     (when (eq term :from)
                       (err "an atom after FOR" varlist))
                     (setf var (ps-gensym))
                     (push (list varlist var) destructurings))
                   (case term
                     (:from (for-from var))
                     (:= (for-= var))
                     ((:in :across) (for-in var))
                     (:on (for-on var))
                     (otherwise (error "FOR ~s ~s is not valid in PS-LOOP." var term)))))
               (clause ()
                 (let ((term (consume-atom)))
                   (case term
                     (:for (for-clause))
                     (:while (push `(unless ,(consume) break) body))
                     (:until (push `(when ,(consume) break) body))
                     (:initially (push (consume-progn) initially))
                     (:finally (push (consume-progn) finally))
                     (:first-time (push (consume-progn) first-time))
                     (:last-time (push (consume-progn) last-time))
                     (otherwise (push (body-clause term) body))))))
        (if terms
            (loop :while terms :do (clause))
            (err "loop definition" nil)))
      (flet ((end-test ()
               (aif (nreverse end-test-forms)
                    (if (cdr it)
                        (list (cons 'or it))
                        it)
                    (list nil)))
             (add-destructurings-to-body ()
               (setf body (nreverse body))
               (loop :for (list var) :in destructurings :do
                 (setf body `((destructuring-bind ,list ,var ,@body))))
               body))
        (values (nreverse prologue)
                (nreverse init-step-forms)
                (end-test)
                (nreverse initially)
                (nreverse finally)
                (nreverse first-time)
                (nreverse last-time)
                default-accum-var
                (add-destructurings-to-body))))))

(defpsmacro loop (&rest args)
  (multiple-value-bind (prologue
                        init-step-forms end-test
                        initially finally
                        first-time last-time
                        default-accum-var
                        body)
      (parse-ps-loop (normalize-loop-keywords args))
    (let ((first-guard (and first-time (ps-gensym)))
          (last-guard (and last-time (ps-gensym))))
      `(,@(if default-accum-var '(with-lambda ()) '(progn))
          ,@(when first-time `((var ,first-guard t)))
          ,@(when last-time `((var ,last-guard nil)))
          ,@prologue
          ,@initially
          (do* ,init-step-forms
               ,end-test
            ,@(when first-time
                    `((when ,first-guard
                        ,@first-time
                        (setf ,first-guard nil))))
            ,@body
            ,@(when last-time
                    `((setf ,last-guard t))))
          ,@(when last-time `((when ,last-guard ,@last-time)))
          ,@finally
          ,@(when default-accum-var `((return ,default-accum-var)))))))
