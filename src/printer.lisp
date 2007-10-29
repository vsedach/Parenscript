(in-package :parenscript)

(defvar *ps-output-stream*)
(defparameter *indent-level* 0)

(defmethod parenscript-print (ps-form &optional *ps-output-stream*)
  (setf *indent-level* 0)
  (flet ((print-ps (form)
           (if (and (listp form) (eql 'js-block (car form))) ;; ignore top-level block
               (loop for (statement . remaining) on (third form) do
                     (ps-print statement) (psw ";") (when remaining (psw #\Newline)))
               (ps-print form))))
    (if *ps-output-stream*
        (print-ps ps-form)
        (with-output-to-string (*ps-output-stream*)
          (print-ps ps-form)))))

(defun psw (obj) ;; parenscript-write
  (princ obj *ps-output-stream*))    

(defgeneric ps-print% (special-form-name special-form-args))

(defmacro defprinter (special-form content-args &body body)
  "Given a special-form name and a destructuring lambda-list for its
arguments, defines a printer for that form using the given body."
  (let ((sf (gensym))
        (sf-args (gensym)))
    `(defmethod ps-print% ((,sf (eql ',special-form)) ,sf-args)
      (declare (ignorable ,sf))
      (destructuring-bind ,content-args
          ,sf-args
        ,@body))))

(defgeneric ps-print (compiled-form))

(defmethod ps-print ((form null)) ;; don't print top-level nils (ex: result of defining macros, etc.)
  )

(defmethod ps-print ((compiled-form cons))
  "Prints the given compiled ParenScript form starting at the given
indent position."
  (ps-print% (car compiled-form) (cdr compiled-form)))

;;; indentation
(defvar *ps-print-pretty* t)
(defvar *indent-num-spaces* 4)

(defun newline-and-indent ()
  (when (and (fresh-line *ps-output-stream*) *ps-print-pretty*)
    (loop repeat (* *indent-level* *indent-num-spaces*)
          do (psw #\Space))))

;;; string literals
(defvar *js-string-delimiter* #\'
  "Specifies which character should be used for delimiting strings.

This variable is used when you want to embed the resulting JavaScript
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
    (psw *js-string-delimiter*)
    (loop for char across string
          for code = (char-code char)
          for special = (lisp-special-char-to-js char)
          do (cond (special (psw #\\) (psw special))
                   ((or (<= code #x1f) (>= code #x80))
                    (format *ps-output-stream* "\\u~4,'0x" code))
                   (t (psw char))))
    (psw *js-string-delimiter*)))

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
      (psw "null")
      (error "Cannot translate quoted value ~S to javascript" val)))

(defprinter js-literal (str)
  (psw str))

(defprinter js-keyword (str)
  (psw str))

(defun print-comma-delimited-list (ps-forms)
  (loop for (form . remaining) on ps-forms do
        (ps-print form) (when remaining (psw ", "))))

(defprinter array-literal (&rest initial-contents)
  (psw #\[) (print-comma-delimited-list initial-contents) (psw #\]))

(defprinter js-aref (array indices)
  (ps-print array)
  (loop for idx in indices do
        (psw #\[) (ps-print idx) (psw #\])))

(defprinter object-literal (&rest slot-definitions)
  (psw #\{)
  (loop for ((key . value) . remaining) on slot-definitions do
        (format *ps-output-stream* "~A: " (js-translate-symbol key))
        (ps-print value)
        (when remaining (psw ", ")))
  (psw " }"))

(defprinter js-variable (var)
  (psw (js-translate-symbol var)))

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
  (psw #\() (ps-print ps-form) (psw #\)))

(defprinter operator (op args)
  (loop for (arg . remaining) on args
        with precedence = (op-precedence op) do
        (if (>= (expression-precedence arg) precedence)
            (parenthesize-print arg)
            (ps-print arg))
        (when remaining (format *ps-output-stream* " ~A " op))))

(defprinter unary-operator (op arg &key prefix)
  (when prefix (psw op))
  (if (and (listp arg) (eql 'operator (car arg)))
      (parenthesize-print arg)
      (ps-print arg))
  (unless prefix (psw op)))

;;; function and method calls
(defprinter js-funcall (fun-designator args)
  (cond ((member (car fun-designator) '(js-variable js-aref js-slot-value))
         (ps-print fun-designator))
        ((eql 'js-lambda (car fun-designator))
         (psw #\() (ps-print fun-designator) (psw #\)))
        ((eql 'js-funcall (car fun-designator))
         (ps-print fun-designator)))
  (psw #\() (print-comma-delimited-list args) (psw #\)))

(defprinter js-method-call (method object args)
  ;; TODO: this may not be the best way to add ()'s around lambdas
  ;; probably there is or should be a more general solution working
  ;; in other situations involving lambda's
  (if (or (numberp object) (and (consp object) (member (car object) '(js-lambda js-object operator js-expression-if))))
      (parenthesize-print object)
      (ps-print object))
  (psw (js-translate-symbol method))
  (psw #\() (print-comma-delimited-list args) (psw #\)))

(defprinter js-block (statement-p statements)
  (if statement-p
      (progn (psw #\{)
             (incf *indent-level*)
             (dolist (statement statements)
               (newline-and-indent) (ps-print statement) (psw #\;))
             (decf *indent-level*)
             (newline-and-indent)
             (psw #\}))
      (progn (psw #\()
             (loop for (statement . remaining) on statements do
                   (ps-print statement) (when remaining (psw ", ")))
             (psw #\)))))

(defprinter js-lambda (args body)
  (print-fun-def nil args body))

(defprinter js-defun (name args body)
  (print-fun-def name args body))

(defun print-fun-def (name args body-block)
  (format *ps-output-stream* "function ~:[~;~A~](" name (js-translate-symbol name))
  (loop for (arg . remaining) on args do
        (psw (js-translate-symbol arg)) (when remaining (psw ", ")))
  (psw ") ")
  (ps-print body-block))

;;; object literals
(defprinter js-object (slot-defs)
  (psw "{ ")
  (loop for ((slot-name slot-value) . remaining) on slot-defs do
        (if (and (listp slot-name) (eql 'script-quote (car slot-name)) (symbolp (second slot-name)))
            (psw (js-translate-symbol (second slot-name)))
            (ps-print slot-name))
        (psw " : ")
        (ps-print slot-value)
        (when remaining (psw ", ")))
  (psw " }"))

(defprinter js-slot-value (obj slot)
  (if (and (listp obj) (member (car obj) '(js-expression-if)))
      (parenthesize-print obj)
      (ps-print obj))
  (if (and (listp slot) (eql 'script-quote (car slot)))
      (progn (psw #\.)
             (if (symbolp (second slot))
                 (psw (js-translate-symbol (second slot)))
                 (ps-print slot)))
      (progn (psw #\[) (ps-print slot) (psw #\]))))

(defprinter js-cond-statement (clauses)
  (loop for (test body-block) in clauses
        for start = "if (" then " else if (" do
        (if (equalp test "true")
            (psw " else ")
            (progn (psw start)
                   (ps-print test)
                   (psw ") ")))
        (ps-print body-block)))

(defprinter js-statement-if (test then-block else-block)
  (psw "if (") (ps-print test) (psw ") ")
  (ps-print then-block)
  (when else-block
      (psw " else ")
      (ps-print else-block)))

(defprinter js-expression-if (test then else)
  (ps-print test)
  (psw " ? ")
  (if (>= (expression-precedence then) (op-precedence 'js-expression-if))
      (parenthesize-print then)
      (ps-print then))
  (psw " : ")
  (if (>= (expression-precedence else) (op-precedence 'js-expression-if))
      (parenthesize-print else)
      (ps-print else)))

(defprinter js-assign (lhs rhs)
  (ps-print lhs) (psw " = ") (ps-print rhs))

(defprinter js-defvar (var-name &rest var-value)
  (psw "var ")
  (psw (js-translate-symbol var-name))
  (when var-value
    (psw " = ")
    (ps-print (car var-value))))

;;; iteration
(defprinter js-for (vars steps test body-block)
  (psw "for (")
  (loop for ((var-name . var-init) . remaining) on vars
        for decl = "var " then "" do
        (psw decl) (psw (js-translate-symbol var-name)) (psw " = ") (ps-print var-init) (when remaining (psw ", ")))
  (psw "; ")
  (ps-print test)
  (psw "; ")
  (loop for ((var-name . nil) . remaining) on vars
        for step in steps do
        (psw (js-translate-symbol var-name)) (psw " = ") (ps-print step) (when remaining (psw ", ")))
  (psw ") ")
  (ps-print body-block))

(defprinter js-for-each (var object body-block)
  (psw "for (var ") (psw (js-translate-symbol var)) (psw " in ") (ps-print object) (psw ") ")
  (ps-print body-block))

(defprinter js-while (test body-block)
  (psw "while (") (ps-print test) (psw ") ")
  (ps-print body-block))

(defprinter js-with (expression body-block)
  (psw "with (") (ps-print expression) (psw ") ")
  (ps-print body-block))

(defprinter js-switch (test clauses)
  (flet ((print-body-statements (body-statements)
           (incf *indent-level*)
           (loop for statement in body-statements do
                 (progn (newline-and-indent)
                        (ps-print statement)
                        (psw #\;)))
           (decf *indent-level*)))
    (psw "switch (") (ps-print test) (psw ") {")
    (loop for (val body-block) in clauses
          for body-statements = (third body-block)
          do (progn (newline-and-indent)
                    (if (eql val 'default)
                        (progn (psw "default: ")
                               (print-body-statements body-statements))
                        (progn (psw "case ")
                               (ps-print val)
                               (psw #\:)
                               (print-body-statements body-statements)))))
    (psw #\})))

(defprinter js-try (body-block &key catch finally)
  (psw "try ")
  (ps-print body-block)
  (when catch
    (psw " catch (") (psw (js-translate-symbol (first catch))) (psw ") ")
    (ps-print (second catch)))
  (when finally
    (psw " finally ")
    (ps-print finally)))

;;; regex
(defprinter js-regex (regex)
  (flet ((first-slash-p (string)
           (and (> (length string) 0) (char= (char string 0) #\/))))
    (let ((slash (unless (first-slash-p regex) "/")))
      (format *ps-output-stream* (concatenate 'string slash "~A" slash) regex))))

(defprinter js-return (value)
  (psw "return ") (ps-print value))

;;; conditional compilation
(defprinter cc-if (test body-forms)
  (psw "/*@if ")
  (ps-print test)
  (incf *indent-level*)
  (dolist (form body-forms)
    (newline-and-indent) (ps-print form) (psw #\;))
  (decf *indent-level*)
  (newline-and-indent)
  (psw "@end @*/"))

(defprinter js-instanceof (value type)
  (psw #\() (ps-print value) (psw " instanceof ") (ps-print type) (psw #\)))

(defprinter js-named-operator (op value)
  (format *ps-output-stream* "~(~A~) " op)
  (ps-print value))
