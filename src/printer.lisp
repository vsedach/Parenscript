(in-package "PARENSCRIPT")

(defvar *ps-print-pretty* t)
(defvar *indent-num-spaces* 4)
(defvar *js-string-delimiter* #\'
  "Specifies which character should be used for delimiting strings.

This variable is used when you want to embed the resulting JavaScript
in an html attribute delimited by #\\\" as opposed to #\\', or
vice-versa.")

(defvar *indent-level*)

(defvar *psw-stream*)

(defun parenscript-print (form immediate?)
  (declare (special immediate?))
  (let ((*indent-level* 0)
        (*psw-stream* (if immediate?
                          *psw-stream*
                          (make-string-output-stream)))
        (%psw-accumulator ()))
    (declare (special %psw-accumulator))
    (if (and (listp form) (eq 'js:block (car form))) ; ignore top-level block
        (loop for (statement . remaining) on (cdr form) do
             (ps-print statement) (psw #\;) (when remaining (psw #\Newline)))
        (ps-print form))
    (unless immediate?
      (reverse (cons (get-output-stream-string *psw-stream*) %psw-accumulator)))))

(defun psw (obj)
  (declare (special %psw-accumulator immediate?))
  (typecase obj
    (string (write-string obj *psw-stream*))
    (character (write-char obj *psw-stream*))
    (otherwise
     (if immediate?
         (write-string (eval obj) *psw-stream*)
         (setf %psw-accumulator
               (cons obj
                     (cons (get-output-stream-string *psw-stream*)
                           %psw-accumulator)))))))

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
  (assert (keywordp s) nil "~S is not a symbol" s)
  (ps-print (string-downcase s)))

(defmethod ps-print ((compiled-form cons))
  (ps-print% (car compiled-form) (cdr compiled-form)))

(defun newline-and-indent ()
  (if *ps-print-pretty*
      (progn (psw #\Newline)
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
                   ((or (<= code #x1f) (>= code #x80)) (format *psw-stream* "\\u~4,'0x" code))
                   (t (psw char))))
    (psw *js-string-delimiter*)))

(defmethod ps-print ((number number))
  (format *psw-stream* (if (integerp number) "~S" "~F") number))

;;; expression and operator precedence rules

(defun expression-precedence (expr)
  (if (consp expr)
      (case (car expr)
        ((js:slot-value js:aref) (op-precedence (car expr)))
        (js:= (op-precedence 'js:=))
        (js:? (op-precedence 'js:?))
        (js:unary-operator (op-precedence (second expr)))
        (operator (op-precedence (second expr)))
        (otherwise -1))
      -1))

(defprinter js:literal (str)
  (psw str))

(defun print-comma-delimited-list (ps-forms)
  (loop for (form . remaining) on ps-forms do
        (ps-print form) (when remaining (psw ", "))))

(defprinter js:array (&rest initial-contents)
  (psw #\[) (print-comma-delimited-list initial-contents) (psw #\]))

(defprinter js:aref (array indices)
  (if (>= (expression-precedence array) (op-precedence 'js:aref))
      (parenthesize-print array)
      (ps-print array))
  (loop for idx in indices do
        (psw #\[) (ps-print idx) (psw #\])))

(defprinter js:variable (var)
  (psw (symbol-to-js-string var)))

;;; arithmetic operators
(defun parenthesize-print (ps-form)
  (psw #\() (ps-print ps-form) (psw #\)))

(defprinter js:operator (op &rest args)
  (loop for (arg . remaining) on args
        with precedence = (op-precedence op) do
        (if (>= (expression-precedence arg) precedence)
            (parenthesize-print arg)
            (ps-print arg))
        (when remaining (format *psw-stream* " ~(~A~) " op))))

(defprinter js:unary-operator (op arg &key prefix space)
  (when prefix (format *psw-stream* "~(~a~)~:[~; ~]" op space))
  (if (> (expression-precedence arg)
         (op-precedence (case op
                          (+ 'unary+)
                          (- 'unary-)
                          (t op))))
      (parenthesize-print arg)
      (ps-print arg))
  (unless prefix (format *psw-stream* "~(~a~)" op)))

(defprinter js:funcall (fun-designator &rest args)
  (funcall (if (member (car fun-designator) '(js:variable js:aref js:slot-value js:funcall))
               #'ps-print
               #'parenthesize-print)
           fun-designator)
  (psw #\() (print-comma-delimited-list args) (psw #\)))

(defprinter js:|,| (&rest expressions)
  (psw #\()
  (loop for (exp . remaining) on expressions do
       (ps-print exp) (when remaining (psw ", ")))
  (psw #\)))

(defprinter js:block (&rest statements)
  (psw #\{)
  (incf *indent-level*)
  (dolist (statement statements)
    (newline-and-indent) (ps-print statement) (psw #\;))
  (decf *indent-level*)
  (newline-and-indent)
  (psw #\}))

(defprinter js:lambda (args body)
  (print-fun-def nil args body))

(defprinter js:defun (name args body)
  (print-fun-def name args body))

(defun print-fun-def (name args body-block)
  (format *psw-stream* "function ~:[~;~A~](" name (symbol-to-js-string name))
  (loop for (arg . remaining) on args do
        (psw (symbol-to-js-string arg)) (when remaining (psw ", ")))
  (psw ") ")
  (ps-print body-block))

(defprinter js:object (&rest slot-defs)
  (psw "{ ")
  (loop for ((slot-name . slot-value) . remaining) on slot-defs do
       (ps-print slot-name) (psw " : ") (ps-print slot-value)
       (when remaining (psw ", ")))
  (psw " }"))

(defprinter js:slot-value (obj slot)
  (if (or (> (expression-precedence obj) (op-precedence 'js:slot-value))
          (numberp obj)
          (and (listp obj) (member (car obj) '(js:lambda js:object))))
      (parenthesize-print obj)
      (ps-print obj))
  (if (and (symbolp slot) (not (keywordp slot)))
      (progn (psw #\.) (psw (symbol-to-js-string slot)))
      (progn (psw #\[) (ps-print slot) (psw #\]))))

(defprinter js:if (test consequent &rest clauses)
  (psw "if (") (ps-print test) (psw ") ")
  (ps-print consequent)
  (loop while clauses do
       (ecase (car clauses)
         (:else-if (psw " else if (") (ps-print (cadr clauses)) (psw ") ")
                   (ps-print (caddr clauses))
                   (setf clauses (cdddr clauses)))
         (:else (psw " else ")
                (ps-print (cadr clauses))
                (return)))))

(defprinter js:? (test then else)
  (if (>= (expression-precedence test) (op-precedence 'js:?))
      (parenthesize-print test)
      (ps-print test))
  (psw " ? ")
  (if (>= (expression-precedence then) (op-precedence 'js:?))
      (parenthesize-print then)
      (ps-print then))
  (psw " : ")
  (if (>= (expression-precedence else) (op-precedence 'js:?))
      (parenthesize-print else)
      (ps-print else)))

(defprinter js:= (lhs rhs)
  (ps-print lhs) (psw " = ") (ps-print rhs))

(defprinter js:var (var-name &rest var-value)
  (psw "var ")
  (psw (symbol-to-js-string var-name))
  (when var-value
    (psw " = ")
    (ps-print (car var-value))))

(defprinter js:break (&optional label)
  (psw "break")
  (when label
    (psw " ")
    (psw (symbol-to-js-string label))))

(defprinter js:continue (&optional label)
  (psw "continue")
  (when label
    (psw " ")
    (psw (symbol-to-js-string label))))

;;; iteration
(defprinter js:for (label vars tests steps body-block)
  (when label (psw (symbol-to-js-string label)) (psw ": ") (newline-and-indent))
  (psw "for (")
  (loop for ((var-name . var-init) . remaining) on vars
        for decl = "var " then "" do
        (psw decl) (psw (symbol-to-js-string var-name)) (psw " = ") (ps-print var-init) (when remaining (psw ", ")))
  (psw "; ")
  (loop for (test . remaining) on tests do
       (ps-print test) (when remaining (psw ", ")))
  (psw "; ")
  (loop for (step . remaining) on steps do
       (ps-print step) (when remaining (psw ", ")))
  (psw ") ")
  (ps-print body-block))

(defprinter js:for-in (var object body-block)
  (psw "for (var ") (ps-print var) (psw " in ")
  (if (> (expression-precedence object) (op-precedence 'in))
      (parenthesize-print object)
      (ps-print object))
  (psw ") ")
  (ps-print body-block))

(defprinter js:while (test body-block)
  (psw "while (") (ps-print test) (psw ") ")
  (ps-print body-block))

(defprinter js:with (expression body-block)
  (psw "with (") (ps-print expression) (psw ") ")
  (ps-print body-block))

(defprinter js:switch (test clauses)
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

(defprinter js:try (body-block &key catch finally)
  (psw "try ")
  (ps-print body-block)
  (when catch
    (psw " catch (") (psw (symbol-to-js-string (first catch))) (psw ") ")
    (ps-print (second catch)))
  (when finally
    (psw " finally ")
    (ps-print finally)))

;;; regex
(defprinter js:regex (regex)
  (let ((slash (unless (and (> (length regex) 0) (char= (char regex 0) #\/)) "/")))
    (psw (concatenate 'string slash regex slash))))

;;; conditional compilation
(defprinter js:cc-if (test &rest body)
  (psw "/*@if ")
  (ps-print test)
  (incf *indent-level*)
  (dolist (form body)
    (newline-and-indent) (ps-print form) (psw #\;))
  (decf *indent-level*)
  (newline-and-indent)
  (psw "@end @*/"))

(defprinter js:instanceof (value type)
  (psw #\()
  (if (> (expression-precedence value) (op-precedence 'js:instanceof))
      (parenthesize-print value)
      (ps-print value))
  (psw " instanceof ")
  (if (> (expression-precedence type) (op-precedence 'js:instanceof))
      (parenthesize-print type)
      (ps-print type))
  (psw #\)))

(defprinter js:escape (literal-js)
  ;; literal-js should be a form that evaluates to a string containing valid JavaScript
  (psw literal-js))

;;; named statements
(defprinter js:throw (x)
  (psw "throw ") (ps-print x))

(defprinter js:return (x)
  (psw "return ") (ps-print x))
