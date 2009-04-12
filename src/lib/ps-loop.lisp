(in-package :parenscript)

(defun complex-js-expr? (expr)
  (if (symbolp expr)
      (find #\. (symbol-name expr))
      (consp expr)))

(defvar *loop-keywords*
  '(:for :do :when :unless :initially :finally :first-time :last-time :while :until
    :from :to :below :downto :above :by :in :across :index := :then :sum :collect))

(defun normalize-loop-keywords (args)
  (mapcar
   (lambda (x)
     (or (find-if (lambda (key) (and (symbolp x) (equal (symbol-name x) (symbol-name key))))
                  *loop-keywords*)
         x))
   args))

(defun parse-ps-loop (terms)
  (let (prologue
        init-step-forms end-test-forms
        initially finally
        first-time last-time
        accum-var accum-kind
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
               (establish-accum-var (kind initial-val)
                 (if accum-var
                     (error "PS-LOOP encountered illegal ~a: a ~a was previously declared, and there can only be one accumulation per loop." kind accum-kind)
                     (progn
                       (setf accum-var (ps-gensym kind)
                             accum-kind kind)
                       (push `(var ,accum-var ,initial-val) prologue))))
               (body-clause (term)
                 (case term
                   ((:when :unless) (list (intern (symbol-name term))
                                          (consume)
                                          (body-clause (consume-atom))))
                   (:sum (establish-accum-var :sum 0)
                         `(incf ,accum-var ,(consume)))
                   (:collect (establish-accum-var :collect '(array))
                     `((@ ,accum-var :push) ,(consume)))
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
               (for-clause ()
                 (let* ((place (consume))
                        (var (when (atom place) place))
                        (varlist (unless var place))
                        (term (consume-atom)))
                   (when varlist
                     (when (eq term :from)
                       (err "an atom after FROM" varlist))
                     (setf var (ps-gensym))
                     (push (list varlist var) destructurings))
                   (case term
                     (:from (for-from var))
                     (:= (for-= var))
                     ((:in :across) (for-in var))
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
                accum-var
                (add-destructurings-to-body))))))

(defpsmacro loop (&rest args)
  (multiple-value-bind (prologue
                        init-step-forms end-test
                        initially finally
                        first-time last-time
                        accum-var
                        body)
      (parse-ps-loop (normalize-loop-keywords args))
    (let ((first-guard (and first-time (ps-gensym)))
          (last-guard (and last-time (ps-gensym))))
      `(,@(if accum-var '(with-lambda ()) '(progn))
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
          ,@(when accum-var `((return ,accum-var)))))))
