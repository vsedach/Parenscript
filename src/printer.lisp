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

(defun psw (&rest objs)
  (dolist (obj objs)
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
                             %psw-accumulator))))))))

(defgeneric ps-print% (js-primitive args))

(defmacro defprinter (js-primitive args &body body)
  (if (listp js-primitive)
      (cons 'progn (mapcar (lambda (p)
                             `(defprinter ,p ,args ,@body))
                           js-primitive))
      (let ((pargs (gensym)))
        `(defmethod ps-print% ((op (eql ',js-primitive)) ,pargs)
           (declare (ignorable op))
           (destructuring-bind ,args
               ,pargs
             ,@(loop for x in body collect
                    (if (or (characterp x)
                            (stringp x))
                        (list 'psw x)
                        x)))))))

(defmethod ps-print ((x null))
  (psw "null"))

(defmethod ps-print ((x (eql t)))
  (psw "true"))

(defmethod ps-print ((x (eql 'js:f)))
  (psw "false"))

(defmethod ps-print ((s symbol))
  (if (keywordp s)
      (ps-print (string-downcase s))
      (psw (symbol-to-js-string s))))

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

(let ((precedence-table (make-hash-table :test 'eq)))
  (loop for level in '((js:getprop js:aref js:new js:funcall)
                       (js:++ js:-- js:post++ js:post--)
                       (js:! js:~ js:negate js:typeof js:delete)
                       (js:* js:/ js:%)
                       (js:+ js:-)
                       (js:<< js:>> js:>>>)
                       (js:< js:> js:<= js:>= js:instanceof js:in)
                       (js:== js:!= js:=== js:!==)
                       (js:&)
                       (js:^)
                       (js:\|)
                       (js:&&)
                       (js:\|\|)
                       (js:?)
                       (js:= js:*= js:/= js:%= js:+= js:-= js:<<= js:>>= js:>>>= js:&= js:^= js:\|=)
                       (js:return js:throw)
                       (js:|,|))
     for i from 0
     do (mapc (lambda (symbol)
                (setf (gethash symbol precedence-table) i))
              level))
  (defun op-precedence (op)
    (gethash op precedence-table -1)))

(defun expression-precedence (expr)
  (if (consp expr)
      (op-precedence (car expr))
      -1))

(defun parenthesize-print (ps-form)
  (psw #\() (ps-print ps-form) (psw #\)))

(defun print-op-argument (op argument)
  (if (< (op-precedence op) (expression-precedence argument))
      (parenthesize-print argument)
      (ps-print argument)))

(defun print-op (op)
  (psw (string-downcase op)))

(defprinter (js:! js:~ js:++ js:--) (x)
  (print-op op) (print-op-argument op x))

(defprinter js:negate (x)
  "-"(print-op-argument op x))

(defprinter (js:delete js:typeof js:new js:throw js:return) (x)
  (print-op op)" "(print-op-argument op x))

(defprinter js:post++ (x)
  (ps-print x)"++")

(defprinter js:post-- (x)
  (ps-print x)"--")

(defprinter (js:+ js:- js:* js:/ js:&& js:\|\| js:& js:\| js:-= js:+= js:*= js:/= js:^ js:= js:== js:=== js:!== js:in js:!= js:> js:>= js:< js:<=)
    (&rest args)
  (loop for (arg . remaining) on args do
       (print-op-argument op arg)
       (when remaining (format *psw-stream* " ~(~A~) " op))))

(defprinter js:aref (array &rest indices)
  (print-op-argument 'js:aref array)
  (dolist (idx indices)
    (psw #\[) (ps-print idx) (psw #\])))

(defun print-comma-delimited-list (ps-forms)
  (loop for (form . remaining) on ps-forms do
        (ps-print form) (when remaining (psw ", "))))

(defprinter js:array (&rest initial-contents)
  "["(print-comma-delimited-list initial-contents)"]")

(defprinter (js:|,|) (&rest expressions)
  (print-comma-delimited-list expressions))

(defprinter js:funcall (fun-designator &rest args)
  ;; JS grammar rules are retarded. There were more exceptions before,
  ;; but I (vsedach) decided they weren't going to occur in "Real Code"
  (if (and (listp fun-designator)
           (eq 'js:lambda (car fun-designator)))
      (parenthesize-print fun-designator)
      (print-op-argument op fun-designator))
  "("(print-comma-delimited-list args)")")

(defprinter js:block (&rest statements)
  "{" (incf *indent-level*)
  (dolist (statement statements)
    (newline-and-indent) (ps-print statement) (psw #\;))
  (decf *indent-level*) (newline-and-indent)
  "}")

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
  "{ "(loop for ((slot-name . slot-value) . remaining) on slot-defs do
           (ps-print slot-name) (psw " : ") (ps-print slot-value)
           (when remaining (psw ", ")))" }")

(defprinter js:getprop (obj slot)
  (print-op-argument op obj)"."(psw (symbol-to-js-string slot)))

(defprinter js:if (test consequent &rest clauses)
  "if ("(ps-print test)") "
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
  (print-op-argument op test) " ? "
  (print-op-argument op then) " : "
  (print-op-argument op else))

(defprinter js:var (var-name &rest var-value)
  "var "(psw (symbol-to-js-string var-name))
  (when var-value
    (psw " = ") (ps-print (car var-value))))

(defprinter js:label (label statement)
  (psw (symbol-to-js-string label))": "(ps-print statement))

(defprinter (js:continue js:break) (&optional label)
  (print-op op) (when label
                  (psw " " (symbol-to-js-string label))))

;;; iteration
(defprinter js:for (vars tests steps body-block)
  (psw "for (")
  (loop for ((var-name . var-init) . remaining) on vars
        for decl = "var " then "" do
        (psw decl (symbol-to-js-string var-name) " = ") (ps-print var-init) (when remaining (psw ", ")))
  "; "
  (loop for (test . remaining) on tests do
       (ps-print test) (when remaining (psw ", ")))
  "; "
  (loop for (step . remaining) on steps do
       (ps-print step) (when remaining (psw ", ")))
  ") "
  (ps-print body-block))

(defprinter js:for-in (var object body-block)
  "for (var "(ps-print var)" in "(ps-print object)") "
  (ps-print body-block))

(defprinter (js:with js:while) (expression body-block)
  (print-op op)" ("(ps-print expression)") "
  (ps-print body-block))

(defprinter js:switch (test clauses)
  "switch ("(ps-print test)") {"
  (flet ((print-body-statements (body-statements)
           (incf *indent-level*)
           (loop for statement in body-statements do
                (progn (newline-and-indent)
                       (ps-print statement)
                       (psw #\;)))
           (decf *indent-level*)))
    (loop for (val . statements) in clauses
       do (progn (newline-and-indent)
                 (if (eq val 'js:default)
                     (progn (psw "default:")
                            (print-body-statements statements))
                     (progn (psw "case ") (ps-print val) (psw #\:)
                            (print-body-statements statements))))))
  (newline-and-indent)
  "}")

(defprinter js:try (body-block &key catch finally)
  "try "(ps-print body-block)
  (when catch
    (psw " catch ("(symbol-to-js-string (first catch))") ")
    (ps-print (second catch)))
  (when finally
    (psw " finally ") (ps-print finally)))

(defprinter js:regex (regex)
  (let ((slash (unless (and (> (length regex) 0) (char= (char regex 0) #\/)) "/")))
    (psw (concatenate 'string slash regex slash))))

(defprinter js:instanceof (value type)
  "("(print-op-argument op value)" instanceof "(print-op-argument op type)")")

(defprinter js:escape (literal-js)
  ;; literal-js should be a form that evaluates to a string containing
  ;; valid JavaScript
  (psw literal-js))
