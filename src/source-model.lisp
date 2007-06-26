(in-package :parenscript)

;;; AST node equality
(defmethod js-equal ((obj1 list) (obj2 list))
  (and (= (length obj1) (length obj2))
       (every #'js-equal obj1 obj2)))

(defmethod js-equal ((obj1 t) (obj2 t))
  (equal obj1 obj2))

(defmacro defjsclass (name superclasses slots &rest class-options)
  (let ((slot-names (mapcar #'(lambda (slot) (if (atom slot) slot (first slot))) slots)))
    `(progn
      (defclass ,name ,superclasses
	,slots ,@class-options)
      (defmethod js-equal ((obj1 ,name) (obj2 ,name))
	(every #'(lambda (slot)
		   (js-equal (slot-value obj1 slot)
			     (slot-value obj2 slot)))
	       ',slot-names)))))

;;; js language types
(defclass statement ()
  ((value :initarg :value :accessor value :initform nil))
  (:documentation "A Javascript entity without a value."))

(defclass expression (statement)
  ()
  (:documentation "A Javascript entity with a value."))

;;; array literals
(defjsclass array-literal (expression)
  ((values :initarg :values :accessor array-values)))

(defjsclass js-aref (expression)
  ((array :initarg :array
	  :accessor aref-array)
   (index :initarg :index
	  :accessor aref-index)))

;;; object literals (maps and hash-tables)
(defjsclass object-literal (expression)
  ((values :initarg :values :accessor object-values)))

;;; string literals
(defjsclass string-literal (expression)
  (value))


;;; number literals
(defjsclass number-literal (expression)
  (value))

;;; variables
(defjsclass js-variable (expression)
  (value))

;;; quote
(defjsclass js-quote (expression)
  ())

;;; operators
(defjsclass op-form (expression)
  ((operator :initarg :operator :accessor operator)
   (args :initarg :args :accessor op-args)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *op-precedence-hash* (make-hash-table :test #'equal))

  ;;; generate the operator precedences from *OP-PRECEDENCES*
  (let ((precedence 1))
    (dolist (ops '((aref)
                   (slot-value)
                   (! not ~)
                   (* / %)
                   (+ -)
                   (<< >>)
                   (>>>)
                   (< > <= >=)
                   (in if)
                   (eql == != =)
                   (=== !==)
                   (&)
                   (^)
                   (\|)
                   (\&\& and)
                   (\|\| or)
                   (setf *= /= %= += -= <<= >>= >>>= \&= ^= \|=)
                   (comma)))
      (dolist (op ops)
        (let ((op-name (symbol-name op)))
          (setf (gethash op-name *op-precedence-hash*) precedence)))
      (incf precedence)))

  (defun op-precedence (op)
    (gethash (if (symbolp op)
                 (symbol-name op)
                 op)
             *op-precedence-hash*)))

(defjsclass one-op (expression)
  ((pre-p :initarg :pre-p
	  :initform nil
	  :accessor one-op-pre-p)
   (op :initarg :op
       :accessor one-op)))

;;; function calls
(defjsclass function-call (expression)
  ((function :initarg :function :accessor f-function)
   (args :initarg :args :accessor f-args)))

(defjsclass method-call (expression)
  ((method :initarg :method :accessor m-method)
   (object :initarg :object :accessor m-object)
   (args :initarg :args :accessor m-args)))

;;; body forms
(defjsclass js-body (expression)
  ((stmts :initarg :stmts :accessor b-stmts)
   (indent :initarg :indent :initform "" :accessor b-indent)))

(defmethod initialize-instance :after ((body js-body) &rest initargs)
  (declare (ignore initargs))
  (let* ((stmts (b-stmts body))
	 (last (last stmts))
	 (last-stmt (car last)))
    (when (typep last-stmt 'js-body)
      (setf (b-stmts body)
	    (nconc (butlast stmts)
		   (b-stmts last-stmt))))))

(defjsclass js-sub-body (js-body)
  (stmts indent))

;;; function definition
(defjsclass js-lambda (expression)
  ((args :initarg :args :accessor lambda-args)
   (body :initarg :body :accessor lambda-body)))

(defjsclass js-defun (js-lambda)
  ((name :initarg :name :accessor defun-name)))

;;; object creation
(defjsclass js-object (expression)
  ((slots :initarg :slots
	  :accessor o-slots)))

(defjsclass js-slot-value (expression)
  ((object :initarg :object
	   :accessor sv-object)
   (slot :initarg :slot
	 :accessor sv-slot)))

;;; cond
(defjsclass js-cond (expression)
  ((tests :initarg :tests
	  :accessor cond-tests)
   (bodies :initarg :bodies
	   :accessor cond-bodies)))

(defjsclass js-if (expression)
  ((test :initarg :test
	 :accessor if-test)
   (then :initarg :then
	 :accessor if-then)
   (else :initarg :else
	 :accessor if-else)))

(defmethod initialize-instance :after ((if js-if) &rest initargs)
  (declare (ignore initargs))
  (when (and (if-then if)
	     (typep (if-then if) 'js-sub-body))
    (change-class (if-then if) 'js-body))
  (when (and (if-else if)
	     (typep (if-else if) 'js-sub-body))
    (change-class (if-else if) 'js-body)))

;;; switch
(defjsclass js-switch (statement)
  ((value :initarg :value :accessor case-value)
   (clauses :initarg :clauses :accessor case-clauses)))

;;; assignment

(defjsclass js-setf (expression)
  ((lhs :initarg :lhs :accessor setf-lhs)
   (rhsides :initarg :rhsides :accessor setf-rhsides)))

;;; defvar
(defjsclass js-defvar (statement)
  ((names :initarg :names :accessor var-names)
   (value :initarg :value :accessor var-value)))

;;; iteration
(defjsclass js-for (statement)
  ((vars :initarg :vars :accessor for-vars)
   (steps :initarg :steps :accessor for-steps)
   (check :initarg :check :accessor for-check)
   (body :initarg :body :accessor for-body)))

(defjsclass for-each (statement)
  ((name :initarg :name :accessor fe-name)
   (value :initarg :value :accessor fe-value)
   (body :initarg :body :accessor fe-body)))

(defjsclass js-while (statement)
  ((check :initarg :check :accessor while-check)
   (body :initarg :body :accessor while-body)))

;;; with
(defjsclass js-with (statement)
  ((obj :initarg :obj :accessor with-obj)
   (body :initarg :body :accessor with-body)))

;;; try-catch
(defjsclass js-try (statement)
  ((body :initarg :body :accessor try-body)
   (catch :initarg :catch :accessor try-catch)
   (finally :initarg :finally :accessor try-finally)))

;;; regular expressions
(defjsclass regex (expression)
  (value))

;;; conditional compilation
(defjsclass cc-if ()
  ((test :initarg :test :accessor cc-if-test)
   (body :initarg :body :accessor cc-if-body)))

;; TODO this may not be the best integrated implementation of
;; instanceof into the rest of the code
(defjsclass js-instanceof (expression)
  ((value)
   (type :initarg :type)))

(defmacro define-js-single-op (name &optional (superclass 'expression))
  (let ((js-name (intern (concatenate 'string "JS-" (symbol-name name)) #.*package*)))
  `(progn
    (defjsclass ,js-name (,superclass)
      (value)))))

(define-js-single-op return statement)
(define-js-single-op throw statement)
(define-js-single-op delete)
(define-js-single-op void)
(define-js-single-op typeof)
(define-js-single-op new)

