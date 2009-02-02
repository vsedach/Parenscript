(in-package :parenscript)

(defvar *ps-print-pretty* t)
(defvar *indent-num-spaces* 4)
(defvar *js-string-delimiter* #\'
  "Specifies which character should be used for delimiting strings.

This variable is used when you want to embed the resulting JavaScript
in an html attribute delimited by #\\\" as opposed to #\\', or
vice-versa.")

(defvar *indent-level*)
(defvar *print-accumulator*)

(defmethod parenscript-print (form)
  (let ((*indent-level* 0)
        (*print-accumulator* ()))
    (if (and (listp form) (eql 'js-block (car form))) ; ignore top-level block
        (loop for (statement . remaining) on (third form) do
             (ps-print statement) (psw ";") (when remaining (psw #\Newline)))
        (ps-print form))
    (reduce (lambda (acc next-token)
              (if (and (stringp next-token)
                       (stringp (car (last acc))))
                  (append (butlast acc) (list (concatenate 'string (car (last acc)) next-token)))
                  (append acc (list next-token))))
            (cons () (reverse *print-accumulator*)))))

(defun psw (obj)
  (push (if (characterp obj) (string obj) obj) *print-accumulator*))

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

(defmethod ps-print ((form null))) ; don't print top-level nils (ex: result of defining macros, etc.)

(defmethod ps-print ((s symbol))
  (assert (keywordp s))
  (ps-print (js-translate-symbol s)))

(defmethod ps-print ((compiled-form cons))
  (ps-print% (car compiled-form) (cdr compiled-form)))

(defun newline-and-indent ()
  (if *ps-print-pretty*
      (when (and (stringp (car *print-accumulator*))
                 (not (char= #\Newline (char (car *print-accumulator*) (1- (length (car *print-accumulator*))))))
                 (psw #\Newline))
        (loop repeat (* *indent-level* *indent-num-spaces*) do (psw #\Space)))
      (psw #\Space)))

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
                   ((or (<= code #x1f) (>= code #x80)) (psw (format nil "\\u~4,'0x" code)))
                   (t (psw char))))
    (psw *js-string-delimiter*)))

(defmethod ps-print ((number number))
  (psw (format nil (if (integerp number) "~S" "~F") number)))

;;; expression and operator precedence rules

(defun expression-precedence (expr)
  (if (consp expr)
      (case (car expr)
        ((js-slot-value js-aref) (op-precedence (car expr)))
        (js-assign (op-precedence '=))
        (js-expression-if (op-precedence 'js-expression-if))
        (unary-operator (op-precedence (second expr)))
        (operator (op-precedence (second expr)))
        (otherwise 0))
      0))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *op-precedence-hash* (make-hash-table :test 'eq))

  (let ((precedence 1))
    (dolist (ops '((new js-slot-value js-aref)
                   (postfix++ postfix--)
                   (delete void typeof ++ -- unary+ unary- ~ !)
                   (* / %)
                   (+ -)
                   (<< >> >>>)
                   (< > <= >= js-instance-of in)
                   (== != === !== eql)
                   (&)
                   (^)
                   (\|)
                   (\&\& and)
                   (\|\| or)
                   (js-expression-if)
                   (= *= /= %= += -= <<= >>= >>>= \&\= ^= \|= js-assign)
                   (comma)))
      (dolist (op ops)
        (setf (gethash op *op-precedence-hash*) precedence))
      (incf precedence)))

  (defun op-precedence (op)
    (gethash op *op-precedence-hash*)))

(defprinter ps-quote (val)
  (if (null val)
      (psw "null")
      (error "Cannot translate quoted value ~S to javascript" val)))

(defprinter js-literal (str)
  (psw str))

(defun print-comma-delimited-list (ps-forms)
  (loop for (form . remaining) on ps-forms do
        (ps-print form) (when remaining (psw ", "))))

(defprinter array-literal (&rest initial-contents)
  (psw #\[) (print-comma-delimited-list initial-contents) (psw #\]))

(defprinter js-aref (array indices)
  (if (>= (expression-precedence array) #.(op-precedence 'js-aref))
      (parenthesize-print array)
      (ps-print array))
  (loop for idx in indices do
        (psw #\[) (ps-print idx) (psw #\])))

(defprinter js-variable (var)
  (psw (js-translate-symbol var)))

;;; arithmetic operators
(defun parenthesize-print (ps-form)
  (psw #\() (ps-print ps-form) (psw #\)))

(defprinter operator (op args)
  (loop for (arg . remaining) on args
        with precedence = (op-precedence op) do
        (if (>= (expression-precedence arg) precedence)
            (parenthesize-print arg)
            (ps-print arg))
        (when remaining (psw (format nil " ~(~A~) " op)))))

(defprinter unary-operator (op arg &key prefix space)
  (when prefix (psw (format nil "~(~a~)~:[~; ~]" op space)))
  (if (> (expression-precedence arg)
         (op-precedence (case op
                          (+ 'unary+)
                          (- 'unary-)
                          (t op))))
      (parenthesize-print arg)
      (ps-print arg))
  (unless prefix (psw (format nil "~(~a~)" op))))

(defprinter js-funcall (fun-designator args)
  (funcall (if (member (car fun-designator) '(js-variable js-aref js-slot-value js-funcall))
               #'ps-print
               #'parenthesize-print)
           fun-designator)
  (psw #\() (print-comma-delimited-list args) (psw #\)))

(defprinter js-block (block-type statements)
  (case block-type
    (:statement
     (psw #\{)
     (incf *indent-level*)
     (dolist (statement statements)
       (newline-and-indent) (ps-print statement) (psw #\;))
     (decf *indent-level*)
     (newline-and-indent)
     (psw #\}))
    (:expression
     (psw #\()
     (loop for (statement . remaining) on statements do
           (ps-print statement) (when remaining (psw ", ")))
     (psw #\)))))

(defprinter js-lambda (args body)
  (print-fun-def nil args body))

(defprinter js-defun (name args body)
  (print-fun-def name args body))

(defun print-fun-def (name args body-block)
  (psw (format nil "function ~:[~;~A~](" name (js-translate-symbol name)))
  (loop for (arg . remaining) on args do
        (psw (js-translate-symbol arg)) (when remaining (psw ", ")))
  (psw ") ")
  (ps-print body-block))

(defprinter js-object (slot-defs)
  (psw "{ ")
  (loop for ((slot-name . slot-value) . remaining) on slot-defs do
        (if (and (listp slot-name) (eql 'ps-quote (car slot-name)) (symbolp (second slot-name)))
            (psw (js-translate-symbol (second slot-name)))
            (ps-print slot-name))
        (psw " : ")
        (ps-print slot-value)
        (when remaining (psw ", ")))
  (psw " }"))

(defprinter js-slot-value (obj slot)
  (if (or (> (expression-precedence obj) #.(op-precedence 'js-slot-value))
          (numberp obj)
          (and (listp obj) (member (car obj) '(js-lambda js-object))))
      (parenthesize-print obj)
      (ps-print obj))
  (if (and (listp slot) (eql 'ps-quote (car slot)))
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

(defprinter js-var (var-name &rest var-value)
  (psw "var ")
  (psw (js-translate-symbol var-name))
  (when var-value
    (psw " = ")
    (ps-print (car var-value))))

(defprinter js-break (&optional label)
  (psw "break")
  (when label
    (psw " ")
    (psw (js-translate-symbol label))))

(defprinter js-continue (&optional label)
  (psw "continue")
  (when label
    (psw " ")
    (psw (js-translate-symbol label))))

;;; iteration
(defprinter js-for (label vars tests steps body-block)
  (when label (psw (js-translate-symbol label)) (psw ": ") (newline-and-indent))
  (psw "for (")
  (loop for ((var-name . var-init) . remaining) on vars
        for decl = "var " then "" do
        (psw decl) (psw (js-translate-symbol var-name)) (psw " = ") (ps-print var-init) (when remaining (psw ", ")))
  (psw "; ")
  (loop for (test . remaining) on tests do
       (ps-print test) (when remaining (psw ", ")))
  (psw "; ")
  (loop for (step . remaining) on steps do
       (ps-print step) (when remaining (psw ", ")))
  (psw ") ")
  (ps-print body-block))

(defprinter js-for-in (var object body-block)
  (psw "for (") (ps-print var) (psw " in ")
  (if (> (expression-precedence object) (op-precedence 'in))
      (parenthesize-print object)
      (ps-print object))
  (psw ") ")
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
    (loop for (val . statements) in clauses
          do (progn (newline-and-indent)
                    (if (eq val 'default)
                        (progn (psw "default: ")
                               (print-body-statements statements))
                        (progn (psw "case ")
                               (ps-print val)
                               (psw #\:)
                               (print-body-statements statements)))))
    (newline-and-indent)
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
      (psw (format nil (concatenate 'string slash "~A" slash) regex)))))

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
  (psw #\()
  (if (> (expression-precedence value) (op-precedence 'js-instance-of))
      (parenthesize-print value)
      (ps-print value))
  (psw " instanceof ")
  (if (> (expression-precedence type) (op-precedence 'js-instance-of))
      (parenthesize-print type)
      (ps-print type))
  (psw #\)))

(defprinter js-escape (lisp-form)
  (psw `(ps1* ,lisp-form)))

;;; named statements
(macrolet ((def-stmt-printer (&rest stmts)
             `(progn ,@(mapcar (lambda (stmt)
                                 `(defprinter ,(intern (format nil "JS-~a" stmt)) (expr)
                                    (psw (format nil "~(~a~) " ',stmt))
                                    (ps-print expr)))
                               stmts))))
  (def-stmt-printer throw return))
