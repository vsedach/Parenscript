(in-package :parenscript)

(defgeneric ps-print% (special-form-name special-form-args %start-pos%))

(defmacro defprinter (special-form content-args &body body)
  "Given a special-form name and a destructuring lambda-list for its
arguments, defines a printer for that form using the given body."
  (let ((sf (gensym))
        (sf-args (gensym)))
    `(defmethod ps-print% ((,sf (eql ',special-form)) ,sf-args %start-pos%)
      (declare (ignore ,sf))
      (destructuring-bind ,content-args
          ,sf-args
        ,@body))))

(defvar %start-pos%)

(defgeneric ps-print (compiled-form %start-pos%))

(defmethod ps-print ((compiled-form cons) %start-pos%)
  "Prints the given compiled ParenScript form starting at the given
indent position."
  (ps-print% (car compiled-form) (cdr compiled-form) %start-pos%))

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

(defmethod ps-print ((string string) %start-pos%)
  (flet ((lisp-special-char-to-js (lisp-char)
           (car (rassoc lisp-char *js-lisp-escaped-chars*))))
    (list (with-output-to-string (escaped)
            (write-char *js-quote-char* escaped)
            (loop for char across string
                  for code = (char-code char)
                  for special = (lisp-special-char-to-js char)
                  do (cond
                       (special
                        (write-char #\\ escaped)
                        (write-char special escaped))
                       ((or (<= code #x1f) (>= code #x80))
                        (format escaped "\\u~4,'0x" code))
                       (t (write-char char escaped)))
                  finally (write-char *js-quote-char* escaped))))))

(defmethod ps-print ((number number) %start-pos%)
  (list (format nil (if (integerp number) "~S" "~F") number)))

;;; expression and operator precedence rules

(defun expression-precedence (expr)
  (if (consp expr)
      (case (car expr)
        (js-block (if (= (length (cdr expr)) 1)
                      (expression-precedence (first (cdr expr)))
                      (op-precedence 'comma)))
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

;;; indenter

(defmacro max-length () '(- 80 %start-pos% 2))

(defun ps-print-indent (ps-form)
  (ps-print ps-form (+ %start-pos% 2)))

(defun special-append-to-last (form elt)
  (flet ((special-append (form elt)
	   (let ((len (length form)))
	     (if (and (> len 0)
                      (string= (char form (1- len)) elt))
		 form
		 (concatenate 'string form elt)))))
    (cond ((stringp form)
	   (special-append form elt))
	  ((consp form)
	   (let ((last (last form)))
	     (if (stringp (car last))
		 (rplaca last (special-append (car last) elt))
		 (append-to-last (car last) elt))
	   form))
	  (t (error "Wrong argument type to indent appender: ~S" form)))))

(defun dwim-join (value-string-lists max-length
		  &key (start "")
                       end
		       (join-before "")
                       join-after
		       (white-space (make-string (length start) :initial-element #\Space))
                       (separator " ")
		  (append-to-last #'append-to-last)
		  (collect t))
    #+nil
    (format t "value-string-lists: ~S~%" value-string-lists)

    ;;; collect single value-string-lists until the line is full

    (do* ((string-lists value-string-lists (cdr string-lists))
	  (string-list (car string-lists) (car string-lists))
	  (cur-elt start)
          (is-first t nil)
	  (cur-empty t)
	  (res nil))
	 ((null string-lists)
	  (unless cur-empty
	    (push cur-elt res))
	  (if (null res)
	      (list (concatenate 'string start end))
	      (progn
		(when end
		  (setf (first res)
			(funcall append-to-last (first res) end)))
		(nreverse res))))
      #+nil
      (format t "string-list: ~S~%" string-list)

      (when join-after
	(unless (null (cdr string-lists))
	  (funcall append-to-last string-list join-after)))

      (if (and collect (= (length string-list) 1))
	  (progn
	    #+nil
	    (format t "cur-elt: ~S line-length ~D, max-length ~D, string: ~S~%"
		    cur-elt
		    (+ (length (first string-list))
		       (length cur-elt))
		    max-length
		    (first string-list))
	    (if (or cur-empty
		    (< (+ (length (first string-list))
			  (length cur-elt)) max-length))
		(setf cur-elt
		      (concatenate 'string cur-elt
				   (if (or is-first (and cur-empty (string= join-before "")))
                                        "" (concatenate 'string separator join-before))
				   (first string-list))
		      cur-empty nil)
		(progn
		  (push cur-elt res)
		  (setf cur-elt (concatenate 'string white-space
					     join-before (first string-list))
			cur-empty nil))))

	  (progn
	    (unless cur-empty
	      (push cur-elt res)
	      (setf cur-elt white-space
		    cur-empty t))
	    (setf res (nconc (nreverse
			      (cons (concatenate 'string
						 cur-elt
                                                 (if (null res)
						     "" join-before)
						 (first string-list))
				    (mapcar #'(lambda (x) (concatenate 'string white-space x))
					    (cdr string-list))))
                             res))
	    (setf cur-elt white-space cur-empty t)))))

(defprinter script-quote (val)
  (if (null val)
      (list "null")
      (error "Cannot translate quoted value ~S to javascript" val)))

(defprinter js-literal (str)
  (list str))

(defprinter js-keyword (str)
  (list str))

;;; array literals

(defprinter array-literal (&rest initial-contents)
  (let ((initial-contents-strings (mapcar #'ps-print-indent initial-contents)))
    (dwim-join initial-contents-strings (max-length)
	       :start "[ " :end " ]"
	       :join-after ",")))

(defprinter js-aref (array coords)
  (dwim-join (cons (ps-print array %start-pos%)
		   (mapcar (lambda (x) (dwim-join (list (ps-print-indent x))
                                                  (max-length)
                                                  :start "[" :end "]"))
			   coords))
	     (max-length)
             :white-space "  "
             :separator ""))

(defprinter object-literal (&rest arrows)
  (dwim-join (loop for (key . value) in arrows appending
                   (list (dwim-join (list (list (format nil "~A:" (js-translate-symbol key)))
                                          (ps-print-indent value))
                                    (max-length)
                                    :start "" :end "" :join-after "")))
             (max-length)
             :start "{ " :end " }"
             :join-after ","))

(defprinter js-variable (var)
  (list (js-translate-symbol var)))

;;; arithmetic operators
(defun script-convert-op-name (op)
  (case op
    (and '\&\&)
    (or '\|\|)
    (not '!)
    (eql '\=\=)
    (=   '\=\=)
    (t op)))

(defun parenthesize (string-list)
  (prepend-to-first string-list "(")
  (append-to-last string-list ")")
  string-list)

(defprinter operator (op args)
  (let* ((precedence (op-precedence op))
	 (arg-strings (mapcar (lambda (arg)
                                (let ((arg-strings (ps-print-indent arg)))
                                  (if (>= (expression-precedence arg) precedence)
                                      (parenthesize arg-strings)
                                      arg-strings)))
                              args))
	 (op-string (format nil "~A " op)))
    (dwim-join arg-strings (max-length) :join-before op-string)))

(defprinter unary-operator (op arg &key prefix)
  (let ((arg-string (ps-print arg %start-pos%)))
    (when (eql 'operator (car arg))
      (setf arg-string (parenthesize arg-string)))
    (if prefix
        (prepend-to-first arg-string op)
        (append-to-last arg-string op))))

;;; function and method calls
(defprinter js-funcall (fun-designator args)
  (let* ((arg-strings (mapcar #'ps-print-indent args))
	 (args (dwim-join arg-strings (max-length)
			  :start "(" :end ")" :join-after ",")))
    (cond ((eql 'js-lambda (car fun-designator))
           (dwim-join (list (append (dwim-join (list (ps-print-indent fun-designator))
                                               (max-length)
                                               :start "(" :end ")" :separator "")
                                    args))
                      (max-length)
                      :separator ""))
          ((member (car fun-designator) '(js-variable js-aref js-slot-value))
           (dwim-join (list (ps-print-indent fun-designator) args)
                      (max-length)
                      :separator ""))
          ((eql 'js-funcall (car fun-designator))
           ;; TODO it adds superfluous newlines after each ()
           ;; and it's nearly the same as the js-lambda case above
           (dwim-join (list (append (dwim-join (list (ps-print-indent fun-designator))
                                               (max-length) :separator "")
                                    args))
                      (max-length) :separator "")))))

(defprinter js-method-call (method object args)
  (let ((printed-object (ps-print object (+ %start-pos% 2))))
    ;; TODO: this may not be the best way to add ()'s around lambdas
    ;; probably there is or should be a more general solution working
    ;; in other situations involving lambda's
    (when (or (numberp object) (and (consp object) (member (car object) '(js-lambda js-object operator))))
      (setf printed-object (append (list "(") printed-object (list ")"))))
    (let* ((fname (dwim-join (list printed-object (list (js-translate-symbol method)))
                             (max-length)
                             :end "("
                             :separator ""))
           (butlast (butlast fname))
           (last (car (last fname)))
           (method-and-args (dwim-join (mapcar #'ps-print-indent args)
                                       (max-length)
                                       :start last
                                       :end ")"
                                       :join-after ","))
           (ensure-no-newline-before-dot (concatenate 'string
                                                      (car (last butlast))
                                                      (first method-and-args))))
      (append (butlast butlast) (list ensure-no-newline-before-dot) (cdr method-and-args)))))

(defprinter js-block (statement-p statements)
  (dwim-join (mapcar #'ps-print-indent statements)
	     (max-length)
	     :join-after (if statement-p ";" ",")
	     :append-to-last #'special-append-to-last
	     :start (if statement-p "    " "")
             :collect nil
	     :end (if statement-p ";" "")))

(defprinter js-lambda (args body)
  (print-fun-def nil args body %start-pos%))

(defprinter js-defun (name args body)
  (print-fun-def name args body %start-pos%))

(defun print-fun-def (name args body %start-pos%)
  (let ((fun-header (dwim-join (mapcar (lambda (x) (list (js-translate-symbol x)))
				       args)
			       (max-length)
			       :start (format nil "function ~:[~;~A~](" name (js-translate-symbol name))
                               :join-after ","
			       :end ") {"))
	(fun-body (ps-print-indent body)))
    (append fun-header fun-body (list "}"))))

;;; object creation
(defprinter js-object (slot-defs)
  (let ((value-string-lists (mapcar (lambda (slot)
                                      (let* ((slot-name (first slot))
                                             (slot-string-name
                                              (if (and (listp slot-name) (eql 'script-quote (car slot-name)))
                                                  (format nil "~A" (if (symbolp (second slot-name))
                                                                       (js-translate-symbol (second slot-name))
                                                                       (car (ps-print slot-name 0))))
                                                  (car (ps-print slot-name 0)))))
                                        (dwim-join (list (ps-print (second slot) (+ %start-pos% 4)))
                                                   (max-length)
                                                   :start (concatenate 'string slot-string-name  " : ")
                                                   :white-space "    ")))
                                    slot-defs)))
    (dwim-join value-string-lists (max-length)
	       :start "{ "
	       :end " }"
	       :join-after ", "
	       :white-space "  "
	       :collect nil)))

(defprinter js-slot-value (obj slot)
  (append-to-last (if (eql 'js-variable (car obj))
                      (ps-print obj %start-pos%)
                      (list (format nil "~A" (ps-print obj %start-pos%))))
                  (if (eql 'script-quote (car slot))
                      (format nil ".~A" (if (symbolp (second slot))
                                            (js-translate-symbol (second slot))
                                            (first (ps-print slot 0))))
                      (format nil "[~A]" (first (ps-print slot 0))))))

;;; cond
(defprinter js-cond (clauses)
  (loop for (test body-forms) in clauses
        for start = "if (" then "else if ("
        append (if (string= test "true")
                   '("else {")
                   (dwim-join (list (ps-print test 0)) (max-length)
                              :start start :end ") {"))
        append (mapcar #'ps-print-indent body-forms)
        collect "}"))

(defprinter js-statement-if (test then else)
  (let ((if-strings (dwim-join (list (ps-print test 0))
			       (- 80 %start-pos% 2)
			       :start "if ("
			       :end ") {"))
	(then-strings (ps-print-indent then))
	(else-strings (when else
			(ps-print-indent else))))
    (append if-strings then-strings (if else-strings
                                        (append (list "} else {") else-strings (list "}"))
                                        (list "}")))))

(defprinter js-expression-if (test then else)
  (dwim-join (list (append-to-last (ps-print test %start-pos%) " ?")
		   (let ((then-string (ps-print then %start-pos%)))
		     (if (>= (expression-precedence then) (op-precedence 'js-expression-if))
                         (parenthesize then-string)
                         then-string))
		   (list ":")
		   (if else
		       (let ((else-string (ps-print else %start-pos%)))
			 (if (>= (expression-precedence else) (op-precedence 'js-expression-if))
			     (parenthesize else-string)
			     else-string))
		       (list "undefined")))
	     (max-length)
	     :white-space "  "))

(defprinter js-assign (lhs rhs)
  (dwim-join (list (ps-print lhs %start-pos%) (ps-print rhs %start-pos%))
	     (max-length)
	     :join-after " ="))

(defprinter js-defvar (var-name &rest var-value)
  (dwim-join (append (list (list (js-translate-symbol var-name)))
                     (when var-value
                       (list (ps-print (car var-value) %start-pos%))))
	     (max-length)
	     :join-after " ="
	     :start "var " :end ";"))

;;; iteration
(defprinter js-for (vars steps test body-block)
  (let* ((init (dwim-join (mapcar (lambda (var-form)
                                    (dwim-join (list (list (js-translate-symbol (car var-form)))
                                                     (ps-print-indent (cdr var-form)))
                                               (max-length)
                                               :join-after " ="))
				  vars)
			  (max-length)
			  :start "var " :join-after ","))
	 (test-string (ps-print-indent test))
	 (step-strings (dwim-join (mapcar (lambda (x var-form)
                                            (dwim-join
                                             (list (list (js-translate-symbol (car var-form)))
                                                   (ps-print x (- %start-pos% 2)))
                                             (max-length)
                                             :join-after " ="))
                                          steps
                                          vars)
                                  (max-length)
                                  :join-after ","))
	 (header (dwim-join (list init test-string step-strings)
			    (max-length)
			    :start "for (" :end ") {"
			    :join-after ";"))
	 (body (ps-print-indent body-block)))
    (append header body (list "}"))))

(defprinter js-for-each (var object body-block)
  (let ((header (dwim-join (list (list (js-translate-symbol var))
				 (list "in")
				 (ps-print-indent object))
			   (max-length)
			   :start "for (var "
			   :end ") {"))
	(body (ps-print-indent body-block)))
    (append header body (list "}"))))

(defprinter js-while (test body-block)
  (let ((header-strings (dwim-join (list (ps-print-indent test))
			   (max-length)
			   :start "while ("
			   :end ") {"))
	(body-strings (ps-print-indent body-block)))
    (append header-strings body-strings (list "}"))))

(defprinter js-with (expression body-block)
  (append (dwim-join (list (ps-print-indent expression))
                     (max-length)
                     :start "with (" :end ") {")
          (ps-print-indent body-block)
          (list "}")))

(defprinter js-switch (test clauses)
  (let ((body-strings (mapcar (lambda (clause)
                                (let ((val (first clause))
                                      (body-block (second clause)))
                                  (dwim-join (list (if (eql val 'default)
                                                       (list "")
                                                       (ps-print-indent val))
                                                   (ps-print-indent body-block))
                                             (max-length)
                                             :start (if (eql val 'default) "  default" "  case ")
                                             :white-space "   "
                                             :join-after ":")))
                              clauses)))
    (append (dwim-join (list (ps-print-indent test))
                       (max-length)
                       :start "switch (" :end ") {")
            (reduce #'append body-strings)
            (list "}"))))

(defprinter js-try (body &key catch finally)
  (let ((catch-strings (when catch
                      (append (dwim-join (list (list (js-translate-symbol (first catch))))
                                         (max-length)
                                         :start "} catch ("
                                         :end ") {")
                              (ps-print-indent (second catch)))))
        (finally-strings (when finally
                           (append (list "} finally {")
                                   (ps-print-indent finally)))))
    (append (list "try {")
            (ps-print-indent body)
            catch-strings
            finally-strings
            (list "}"))))

;;; regex
(defprinter js-regex (regex)
  (flet ((first-slash-p (string)
           (and (> (length string) 0) (eql (char string 0) '#\/))))
    (let ((slash (unless (first-slash-p regex) "/")))
      (list (format nil (concatenate 'string slash "~A" slash) regex)))))

(defprinter js-return (value)
  (let ((printed-value (ps-print value 0)))
    (cons (concatenate 'string "return " (car printed-value)) (cdr printed-value))))

;;; conditional compilation
(defprinter cc-if (test body-forms)
  (append (list (format nil "/*@if ~A" test))
          (mapcar (lambda (x) (ps-print x %start-pos%)) body-forms)
          (list "@end @*/")))

;;; TODO instanceof
(defprinter js-instanceof (value type)
  (dwim-join (list (ps-print-indent value)
                   (list "instanceof")
                   (ps-print-indent type))
             (max-length)
             :start "("
             :end ")"
             :white-space "  "))

(defprinter js-named-operator (op value)
  (dwim-join (list (ps-print-indent value))
             (max-length)
             :start (concatenate 'string (string-downcase (symbol-name op)) " ")
             :white-space "  "))
