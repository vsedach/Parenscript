(in-package :parenscript)

(defvar *ps-output-stream*)
(defparameter *indent-level* 0)

(defmethod parenscript-print (ps-form &optional *ps-output-stream*)
  (setf *indent-level* 0)
  (flet ((print-ps (form)
           (let ((*standard-output* *ps-output-stream*))
             (if (and (listp form) (eql 'js-block (car form))) ;; ignore top-level block
                 (dolist (statement (third form))
                   (ps-print statement)
                   (format *ps-output-stream* ";~%"))
                 (ps-print form)))))
    (if *ps-output-stream*
        (print-ps ps-form)
        (with-output-to-string (*ps-output-stream*)
          (print-ps ps-form)))))

(defgeneric ps-print% (special-form-name special-form-args))

(defmacro defprinter (special-form content-args &body body)
  "Given a special-form name and a destructuring lambda-list for its
arguments, defines a printer for that form using the given body."
  (let ((sf (gensym))
        (sf-args (gensym)))
    `(defmethod ps-print% ((,sf (eql ',special-form)) ,sf-args)
      (declare (ignore ,sf))
      (destructuring-bind ,content-args
          ,sf-args
        ,@body))))

(defgeneric ps-print (compiled-form))

(defmethod ps-print ((form null)) ;; don't print nils (ex: result of defining macros, etc.)
  )

(defmethod ps-print ((compiled-form cons))
  "Prints the given compiled ParenScript form starting at the given
indent position."
  (ps-print% (car compiled-form) (cdr compiled-form)))

;;; indenter

(defparameter *indent-num-space* 4)

(defun newline-and-indent ()
  (when (fresh-line)
    (loop repeat (* *indent-level* *indent-num-space*)
          do (write-char #\Space))))

;;; string literals
(defvar *js-quote-char* #\'
  "Specifies which character JS should use for delimiting strings.

This variable is useful when have to embed some javascript code
in an html attribute delimited by #\\\" as opposed to #\\', or
vice-versa.")

(defparameter *js-lisp-escaped-chars*
  '((#\' . #\')
    (#\\ . #\\)
    (#\b . #\Backspace)
    (#\f . #.(code-char 12))
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)))

(defmethod ps-print ((string string))
  (flet ((lisp-special-char-to-js (lisp-char)
           (car (rassoc lisp-char *js-lisp-escaped-chars*))))
    (write-char *js-quote-char*)
    (loop for char across string
          for code = (char-code char)
          for special = (lisp-special-char-to-js char)
          do (cond (special (write-char #\\)
                            (write-char special))
                   ((or (<= code #x1f) (>= code #x80))
                    (format *ps-output-stream* "\\u~4,'0x" code))
                   (t (write-char char)))
          finally (write-char *js-quote-char*))))

(defmethod ps-print ((number number))
  (format *ps-output-stream* (if (integerp number) "~S" "~F") number))

;;; expression and operator precedence rules

(defun expression-precedence (expr)
  (if (consp expr)
      (case (car expr)
        (js-expression-if (op-precedence 'js-expression-if))
        (js-assign (op-precedence '=))
        (operator (op-precedence (second expr)))
        (otherwise 0))
      0))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *op-precedence-hash* (make-hash-table :test #'equal))

  ;;; generate the operator precedences from *OP-PRECEDENCES*
  (let ((precedence 1))
    (dolist (ops '((js-aref)
                   (js-slot-value)
                   (! not ~)
                   (* / %)
                   (+ -)
                   (<< >>)
                   (>>>)
                   (< > <= >=)
                   (in js-expression-if)
                   (eql == != =)
                   (=== !==)
                   (&)
                   (^)
                   (\|)
                   (\&\& and)
                   (\|\| or)
                   (js-assign *= /= %= += -= <<= >>= >>>= \&= ^= \|=)
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

(defprinter script-quote (val)
  (if (null val)
      (write-string "null")
      (error "Cannot translate quoted value ~S to javascript" val)))

(defprinter js-literal (str)
  (write-string str))

(defprinter js-keyword (str)
  (write-string str))

(defun print-comma-list (ps-forms)
  (loop for (form . rest) on ps-forms
        with after = ", "
        unless rest do (setf after "")
        doing (progn (ps-print form)
                     (write-string after))))

(defprinter array-literal (&rest initial-contents)
  (write-char #\[)
  (print-comma-list initial-contents)
  (write-char #\]))

(defprinter js-aref (array indices)
  (ps-print array)
  (loop for idx in indices do
        (progn (write-char #\[)
               (ps-print idx)
               (write-char #\]))))

(defprinter object-literal (&rest slot-definitions)
  (write-char #\{)
  (loop for ((key . value) . rest) on slot-definitions
        with after = ", "
        unless rest do (setf after "")
        doing (progn (format *ps-output-stream* "~A: " (js-translate-symbol key))
                     (ps-print value)
                     (write-string after)))
  (write-string " }"))

(defprinter js-variable (var)
  (write-string (js-translate-symbol var)))

;;; arithmetic operators
(defun script-convert-op-name (op)
  (case op
    (and '\&\&)
    (or '\|\|)
    (not '!)
    (eql '\=\=)
    (=   '\=\=)
    (t op)))

(defun parenthesize-print (ps-form)
  (write-char #\()
  (ps-print ps-form)
  (write-char #\)))

(defprinter operator (op args)
  (loop for (arg . rest) on args
        with precedence = (op-precedence op)
        with op-string = (format nil " ~A " op)
        unless rest do (setf op-string "")
        do (progn (if (>= (expression-precedence arg) precedence)
                      (parenthesize-print arg)
                      (ps-print arg))
                  (write-string op-string))))

(defprinter unary-operator (op arg &key prefix)
  (when prefix
    (write-string op))
  (if (and (listp arg) (eql 'operator (car arg)))
      (parenthesize-print arg)
      (ps-print arg))
  (unless prefix
    (write-string op)))

;;; function and method calls
(defprinter js-funcall (fun-designator args)
  (cond ((member (car fun-designator) '(js-variable js-aref js-slot-value))
         (ps-print fun-designator))
        ((eql 'js-lambda (car fun-designator))
         (write-char #\()
         (ps-print fun-designator)
         (write-char #\)))
        ((eql 'js-funcall (car fun-designator))
         (ps-print fun-designator)))
  (write-char #\()
  (print-comma-list args)
  (write-char #\)))

(defprinter js-method-call (method object args)
  ;; TODO: this may not be the best way to add ()'s around lambdas
  ;; probably there is or should be a more general solution working
  ;; in other situations involving lambda's
  (if (or (numberp object) (and (consp object) (member (car object) '(js-lambda js-object operator js-expression-if))))
      (parenthesize-print object)
      (ps-print object))
  (write-string (js-translate-symbol method))
  (write-char #\()
  (print-comma-list args)
  (write-char #\)))

(defprinter js-block (statement-p statements)
  (if statement-p
      (progn (write-char #\{)
             (incf *indent-level*)
             (loop for statement in statements
                   do (progn (newline-and-indent)
                             (ps-print statement)
                             (write-char #\;)))
             (decf *indent-level*)
             (newline-and-indent)
             (write-char #\}))
      (progn (write-char #\()
             (loop for (statement . rest) on statements
                   with after = ", "
                   unless rest do (setf after "")
                   do (progn (ps-print statement)
                             (write-string after)))
             (write-char #\)))))

(defprinter js-lambda (args body)
  (print-fun-def nil args body))

(defprinter js-defun (name args body)
  (print-fun-def name args body))

(defun print-fun-def (name args body-block)
  (format *ps-output-stream* "function ~:[~;~A~](" name (js-translate-symbol name))
  (loop for (arg . rest) on args
        with after = ", "
        unless rest do (setf after "")
        do (progn (write-string (js-translate-symbol arg))
                  (write-string after))
        finally (write-string ") "))
  (ps-print body-block))

;;; object creation
(defprinter js-object (slot-defs)
  (write-string "{ ")
  (loop for ((slot-name slot-value) . rest) on slot-defs
        with after = ", "
        unless rest do (setf after "")
        do (progn (if (and (listp slot-name) (eql 'script-quote (car slot-name)) (symbolp (second slot-name)))
                      (write-string (js-translate-symbol (second slot-name)))
                      (ps-print slot-name))
                  (write-string " : ")
                  (ps-print slot-value)
                  (write-string after)))
  (write-string " }"))

(defprinter js-slot-value (obj slot)
  (if (and (listp obj) (member (car obj) '(js-expression-if)))
      (parenthesize-print obj)
      (ps-print obj))
  (if (and (listp slot) (eql 'script-quote (car slot)))
      (progn (write-char #\.)
             (if (symbolp (second slot))
                 (write-string (js-translate-symbol (second slot)))
                 (ps-print slot)))
      (progn (write-char #\[)
             (ps-print slot)
             (write-char #\]))))

;;; cond
(defprinter js-cond-statement (clauses)
  (loop for (test body-block) in clauses
        for start = "if (" then " else if ("
        do (progn (if (equalp test "true")
                      (write-string " else ")
                      (progn (write-string start)
                             (ps-print test)
                             (write-string ") ")))
                  (ps-print body-block))))

(defprinter js-statement-if (test then-block else-block)
  (write-string "if (")
  (ps-print test)
  (write-string ") ")
  (ps-print then-block)
  (when else-block
      (write-string " else ")
      (ps-print else-block)))

(defprinter js-expression-if (test then else)
  (ps-print test)
  (write-string " ? ")
  (if (>= (expression-precedence then) (op-precedence 'js-expression-if))
      (parenthesize-print then)
      (ps-print then))
  (write-string " : ")
  (if (>= (expression-precedence else) (op-precedence 'js-expression-if))
      (parenthesize-print else)
      (ps-print else)))

(defprinter js-assign (lhs rhs)
  (ps-print lhs)
  (write-string " = ")
  (ps-print rhs))

(defprinter js-defvar (var-name &rest var-value)
  (write-string "var ")
  (write-string (js-translate-symbol var-name))
  (when var-value
    (write-string " = ")
    (ps-print (car var-value))))

;;; iteration
(defprinter js-for (vars steps test body-block)
  (write-string "for (")
  (loop for ((var-name . var-init) . rest) on vars
        for decl = "var " then ""
        with after = ", "
        unless rest do (setf after "")
        do (progn (write-string decl)
                  (write-string (js-translate-symbol var-name))
                  (write-string " = ")
                  (ps-print var-init)
                  (write-string after)))
  (write-string "; ")
  (ps-print test)
  (write-string "; ")
  (loop for ((var-name . nil) . rest) on vars
        for step in steps
        with after = ", "
        unless rest do (setf after "")
        do (progn (write-string (js-translate-symbol var-name))
                  (write-string " = ")
                  (ps-print step)
                  (write-string after)))
  (write-string ") ")
  (ps-print body-block))

(defprinter js-for-each (var object body-block)
  (write-string "for (var ")
  (write-string (js-translate-symbol var))
  (write-string " in ")
  (ps-print object)
  (write-string ") ")
  (ps-print body-block))

(defprinter js-while (test body-block)
  (write-string "while (")
  (ps-print test)
  (write-string ") ")
  (ps-print body-block))

(defprinter js-with (expression body-block)
  (write-string "with (")
  (ps-print expression)
  (write-string ") ")
  (ps-print body-block))

(defprinter js-switch (test clauses)
  (flet ((print-body-statements (body-statements)
           (incf *indent-level*)
           (loop for statement in body-statements do
                 (progn (newline-and-indent)
                        (ps-print statement)
                        (write-char #\;)))
           (decf *indent-level*)))
    (write-string "switch (")
    (ps-print test)
    (write-string ") {")
    (loop for (val body-block) in clauses
          for body-statements = (third body-block)
          do (progn (newline-and-indent)
                    (if (eql val 'default)
                        (progn (write-string "default: ")
                               (print-body-statements body-statements))
                        (progn (write-string "case ")
                               (ps-print val)
                               (write-char #\:)
                               (print-body-statements body-statements)))))
    (write-char #\})))

(defprinter js-try (body-block &key catch finally)
  (write-string "try ")
  (ps-print body-block)
  (when catch
    (write-string " catch (")
    (write-string (js-translate-symbol (first catch)))
    (write-string ") ")
    (ps-print (second catch)))
  (when finally
    (write-string " finally ")
    (ps-print finally)))

;;; regex
(defprinter js-regex (regex)
  (flet ((first-slash-p (string)
           (and (> (length string) 0) (char= (char string 0) #\/))))
    (let ((slash (unless (first-slash-p regex) "/")))
      (format *ps-output-stream* (concatenate 'string slash "~A" slash) regex))))

(defprinter js-return (value)
  (write-sequence "return " *ps-output-stream*)
  (ps-print value))

;;; conditional compilation
(defprinter cc-if (test body-forms)
  (write-string "/*@if ")
  (ps-print test)
  (incf *indent-level*)
  (dolist (form body-forms)
    (newline-and-indent)    
    (ps-print form)
    (write-char #\;))
  (decf *indent-level*)
  (newline-and-indent)
  (write-string "@end @*/"))

(defprinter js-instanceof (value type)
  (write-char #\()
  (ps-print value)
  (write-string " instanceof ")
  (ps-print type)
  (write-char #\)))

(defprinter js-named-operator (op value)
  (format *ps-output-stream* "~(~A~) " op)
  (ps-print value))
