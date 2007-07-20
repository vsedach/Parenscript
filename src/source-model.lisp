(in-package :parenscript)

(defgeneric script-equal (compiled-ast-node1 compiled-ast-node2)
  (:documentation "Determines if the AST nodes are equal."))

(defgeneric expression-precedence (expression)
  (:documentation "Returns the precedence of an enscript-javascript expression"))

;;; AST node equality
(defmethod script-equal ((obj1 list) (obj2 list))
  (and (= (length obj1) (length obj2))
       (every #'script-equal obj1 obj2)))

(defmethod script-equal ((obj1 t) (obj2 t))
  (equal obj1 obj2))

(defmacro defscriptclass (name superclasses slots &rest class-options)
  (let ((slot-names (mapcar #'(lambda (slot) (if (atom slot) slot (first slot))) slots)))
    `(progn
      (defclass ,name ,superclasses
	,slots ,@class-options)
      (defmethod script-equal ((obj1 ,name) (obj2 ,name))
	(every #'(lambda (slot)
		   (script-equal (slot-value obj1 slot)
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
(defscriptclass array-literal (expression)
  ((values :initarg :values :accessor array-values)))

(defscriptclass script-aref (expression)
  ((array :initarg :array
	  :accessor aref-array)
   (index :initarg :index
	  :accessor aref-index)))

;;; object literals (maps and hash-tables)
(defscriptclass object-literal (expression)
  ((values :initarg :values :accessor object-values)))

;;; string literals
(defscriptclass string-literal (expression)
  (value))


;;; number literals
(defscriptclass number-literal (expression)
  (value))

;;; variables
(defscriptclass script-variable (expression)
  (value))

;;; quote
(defscriptclass script-quote (expression)
  ())

;;; operators
(defscriptclass op-form (expression)
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

(defscriptclass one-op (expression)
  ((pre-p :initarg :pre-p
	  :initform nil
	  :accessor one-op-pre-p)
   (op :initarg :op
       :accessor one-op)))

;;; function calls
(defscriptclass function-call (expression)
  ((function :initarg :function :accessor f-function)
   (args :initarg :args :accessor f-args)))

(defscriptclass method-call (expression)
  ((method :initarg :method :accessor m-method)
   (object :initarg :object :accessor m-object)
   (args :initarg :args :accessor m-args)))

;;; body forms
(defscriptclass script-body (expression)
  ((statements :initarg :statements :accessor b-statements)
   (indent :initarg :indent :initform "" :accessor b-indent)))

(defmethod initialize-instance :after ((body script-body) &rest initargs)
  (declare (ignore initargs))
  (let* ((statements (b-statements body))
	 (last (last statements))
	 (last-stmt (car last)))
    (when (typep last-stmt 'script-body)
      (setf (b-statements body)
	    (nconc (butlast statements)
		   (b-statements last-stmt))))))

(defscriptclass script-sub-body (script-body)
  (statements indent))

;;; function definition
(defscriptclass script-lambda (expression)
  ((args :initarg :args :accessor lambda-args)
   (body :initarg :body :accessor lambda-body)))

(defscriptclass script-defun (script-lambda)
  ((name :initarg :name :accessor defun-name)))

;;; object creation
(defscriptclass script-object (expression)
  ((slots :initarg :slots
	  :accessor o-slots)))

(defscriptclass script-slot-value (expression)
  ((object :initarg :object
	   :accessor sv-object)
   (slot :initarg :slot
	 :accessor sv-slot)))

;;; cond
(defscriptclass script-cond (expression)
  ((tests :initarg :tests
	  :accessor cond-tests)
   (bodies :initarg :bodies
	   :accessor cond-bodies)))

(defscriptclass script-if (expression)
  ((test :initarg :test
	 :accessor if-test)
   (then :initarg :then
	 :accessor if-then)
   (else :initarg :else
	 :accessor if-else)))

(defmethod initialize-instance :after ((if script-if) &rest initargs)
  (declare (ignore initargs))
  (when (and (if-then if)
	     (typep (if-then if) 'script-sub-body))
    (change-class (if-then if) 'script-body))
  (when (and (if-else if)
	     (typep (if-else if) 'script-sub-body))
    (change-class (if-else if) 'script-body)))

;;; switch
(defscriptclass script-switch (statement)
  ((value :initarg :value :accessor case-value)
   (clauses :initarg :clauses :accessor case-clauses)))

;;; assignment

(defscriptclass script-setf (expression)
  ((lhs :initarg :lhs :accessor setf-lhs)
   (rhsides :initarg :rhsides :accessor setf-rhsides)))

;;; defvar
(defscriptclass script-defvar (statement)
  ((names :initarg :names :accessor var-names)
   (value :initarg :value :accessor var-value)))

;;; iteration
(defscriptclass script-for (statement)
  ((vars :initarg :vars :accessor for-vars)
   (steps :initarg :steps :accessor for-steps)
   (check :initarg :check :accessor for-check)
   (body :initarg :body :accessor for-body)))

(defscriptclass for-each (statement)
  ((name :initarg :name :accessor fe-name)
   (value :initarg :value :accessor fe-value)
   (body :initarg :body :accessor fe-body)))

(defscriptclass script-while (statement)
  ((check :initarg :check :accessor while-check)
   (body :initarg :body :accessor while-body)))

;;; with
(defscriptclass script-with (statement)
  ((obj :initarg :obj :accessor with-obj)
   (body :initarg :body :accessor with-body)))

;;; try-catch
(defscriptclass script-try (statement)
  ((body :initarg :body :accessor try-body)
   (catch :initarg :catch :accessor try-catch)
   (finally :initarg :finally :accessor try-finally)))

;;; regular expressions
(defscriptclass regex (expression)
  (value))

;;; conditional compilation
(defscriptclass cc-if ()
  ((test :initarg :test :accessor cc-if-test)
   (body :initarg :body :accessor cc-if-body)))

;; TODO this may not be the best integrated implementation of
;; instanceof into the rest of the code
(defscriptclass script-instanceof (expression)
  ((value)
   (type :initarg :type)))

(defmacro define-script-single-op (name &optional (superclass 'expression))
  (let ((script-name (intern (concatenate 'string "SCRIPT-" (symbol-name name)) #.*package*)))
  `(progn
    (defscriptclass ,script-name (,superclass)
      (value)))))

(define-script-single-op return statement)
(define-script-single-op throw statement)
(define-script-single-op delete)
(define-script-single-op void)
(define-script-single-op typeof)
(define-script-single-op new)

;;; for script-package stuff
(defscriptclass blank-statement (statement)
  ()
  (:documentation "An empty statement that does nothing."))