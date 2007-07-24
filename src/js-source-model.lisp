(in-package :parenscript)

(defgeneric script-equal (compiled-ast-node1 compiled-ast-node2)
  (:documentation "Determines if the AST nodes are equal."))

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

(in-package :parenscript.javascript)

(defgeneric expression-precedence (expression)
  (:documentation "Returns the precedence of an enscript-javascript expression"))

;;;; define Javascript language types
(defclass statement ()
  ((value :initarg :value :accessor value :initform nil))
  (:documentation "A Javascript entity without a value."))

(defclass expression (statement)
  ()
  (:documentation "A Javascript entity with a value."))

;;; array literals
(defscriptclass array-literal (expression)
  ((values :initarg :values :accessor array-values)))

(defscriptclass js-aref (expression)
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
(defscriptclass js-variable (expression)
  (value))

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
(defscriptclass js-block (expression)
  ((statements :initarg :statements :accessor block-statements)
   (indent :initarg :indent :initform "" :accessor block-indent)))

(defmethod initialize-instance :after ((block js-block) &rest initargs)
  (declare (ignore initargs))
  (let* ((statements (block-statements block))
	 (last (last statements))
	 (last-stmt (car last)))
    (when (typep last-stmt 'js-block)
      (setf (block-statements block)
	    (nconc (butlast statements)
		   (block-statements last-stmt))))))

(defscriptclass js-sub-block (js-block)
  (statements indent))

;;; function definition
(defscriptclass js-lambda (expression)
  ((args :initarg :args :accessor lambda-args)
   (body :initarg :body :accessor lambda-body)))

(defscriptclass js-defun (js-lambda)
  ((name :initarg :name :accessor defun-name)))

;;; object creation
(defscriptclass js-object (expression)
  ((slots :initarg :slots
	  :accessor o-slots)))

(defscriptclass js-slot-value (expression)
  ((object :initarg :object
	   :accessor sv-object)
   (slot :initarg :slot
	 :accessor sv-slot)))

;;; cond
(defscriptclass js-cond (expression)
  ((tests :initarg :tests
	  :accessor cond-tests)
   (bodies :initarg :bodies
	   :accessor cond-bodies)))

(defscriptclass js-if (expression)
  ((test :initarg :test
	 :accessor if-test)
   (then :initarg :then
	 :accessor if-then)
   (else :initarg :else
	 :accessor if-else)))

(defmethod initialize-instance :after ((if js-if) &rest initargs)
  (declare (ignore initargs))
  (when (and (if-then if)
	     (typep (if-then if) 'js-sub-block))
    (change-class (if-then if) 'js-block))
  (when (and (if-else if)
	     (typep (if-else if) 'js-sub-block))
    (change-class (if-else if) 'js-block)))

;;; switch
(defscriptclass js-switch (statement)
  ((value :initarg :value :accessor case-value)
   (clauses :initarg :clauses :accessor case-clauses)))

;;; assignment

(defscriptclass js-setf (expression)
  ((lhs :initarg :lhs :accessor setf-lhs)
   (rhsides :initarg :rhsides :accessor setf-rhsides)))

;;; defvar
(defscriptclass js-defvar (statement)
  ((names :initarg :names :accessor var-names)
   (value :initarg :value :accessor var-value)))

;;; iteration
(defscriptclass js-for (statement)
  ((vars :initarg :vars :accessor for-vars)
   (steps :initarg :steps :accessor for-steps)
   (check :initarg :check :accessor for-check)
   (body :initarg :body :accessor for-body)))

(defscriptclass for-each (statement)
  ((name :initarg :name :accessor fe-name)
   (value :initarg :value :accessor fe-value)
   (body :initarg :body :accessor fe-body)))

(defscriptclass js-while (statement)
  ((check :initarg :check :accessor while-check)
   (body :initarg :body :accessor while-body)))

;;; with
(defscriptclass js-with (statement)
  ((obj :initarg :obj :accessor with-obj)
   (body :initarg :body :accessor with-body)))

;;; try-catch
(defscriptclass js-try (statement)
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
(defscriptclass js-instanceof (expression)
  ((value)
   (type :initarg :type)))

(defmacro define-js-single-op (name &optional (superclass 'expression))
  (let ((js-name (intern (concatenate 'string "JS-" (symbol-name name)) #.*package*)))
  `(progn
    (defscriptclass ,js-name (,superclass)
      (value)))))

(define-js-single-op return statement)
(define-js-single-op throw statement)
(define-js-single-op delete)
(define-js-single-op void)
(define-js-single-op typeof)
(define-js-single-op new)