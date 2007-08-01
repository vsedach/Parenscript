(in-package :parenscript.javascript)

(defgeneric js-to-strings (expression start-pos)
  (:documentation "Transform an enscript-javascript expression to a string"))

(defgeneric js-to-statement-strings (code-fragment start-pos)
  (:documentation "Transform an enscript-javascript code fragment to a string"))

;;; indenter

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
	  (t (error "unsupported form ~S" form)))))

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

    ;;; collect single value-string-lists until line full

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

(defmethod js-to-strings ((expression expression) start-pos)
  (declare (ignore start-pos))
  (list (princ-to-string (value expression))))

(defmethod js-to-statement-strings ((expression expression) start-pos)
  (js-to-strings expression start-pos))

(defmethod js-to-statement-strings ((statement statement) start-pos)
  (declare (ignore start-pos))
  (list (princ-to-string (value statement))))

(defmethod js-to-strings ((expression script-quote) start-pos)
  (declare (ignore start-pos))
  (list
   (if (null (value expression))
       "null"
       (case (value expression)
	 (t (error "Cannot translated quoted value ~A to javascript" (value expression)))))))

;;; array literals

(defmethod js-to-strings ((array array-literal) start-pos)
  (let ((value-string-lists
	 (mapcar #'(lambda (x) (js-to-strings x (+ start-pos 2)))
		 (array-values array)))
	(max-length (- 80 start-pos 2)))
    (dwim-join value-string-lists max-length
	       :start "[ " :end " ]"
	       :join-after ",")))

(defmethod js-to-strings ((aref js-aref) start-pos)
  (dwim-join (cons (js-to-strings (aref-array aref) start-pos)
		   (mapcar #'(lambda (x) (dwim-join (list (js-to-strings x (+ start-pos 2)))
						    (- 80 start-pos 2)
						    :start "[" :end "]"))
			   (aref-index aref)))
	     (- 80 start-pos 2) :separator ""
	     :white-space "  "))

;;; object literals (maps and hash-tables)

(defmethod js-to-strings ((obj object-literal) start-pos)
  (dwim-join
   (loop
    for (key . value) in (object-values obj)
    append (list
	    (dwim-join (list (list (format nil "~A:" (js-translate-symbol key)))
			     (js-to-strings value (+ start-pos 2)))
		       (- 80 start-pos 2)
		       :start "" :end "" :join-after "")))
   (- 80 start-pos 2)
   :start "{ " :end " }"
   :join-after ","))

;;; string literals

(defvar *js-quote-char* #\'
  "Specifies which character JS sholud use for delimiting strings.

This variable is usefull when have to embed some javascript code
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

(defun lisp-special-char-to-js (lisp-char)
    (car (rassoc lisp-char *js-lisp-escaped-chars*)))

(defmethod js-to-strings ((string string-literal) start-pos)
  (declare (ignore start-pos)
           (inline lisp-special-char-to-js))
  (list (with-output-to-string (escaped)
          (write-char *js-quote-char*  escaped)
          (loop
           for char across (value string)
           for code = (char-code char)
           for special = (lisp-special-char-to-js char)
           do
           (cond
             (special
              (write-char #\\ escaped)
              (write-char special escaped))
             ((or (<= code #x1f) (>= code #x80))
              (format escaped "\\u~4,'0x" code))
             (t (write-char char escaped)))
           finally (write-char *js-quote-char* escaped)))))

;;; variables
(defgeneric js-translate-symbol-contextually (symbol package env)
  (:documentation "Translates a symbol to a string in the given environment & package
and for the given symbol."))

(defparameter *obfuscate-standard-identifiers* nil)

(defparameter *obfuscation-table* (make-hash-table))

(defun obfuscated-symbol (symbol)
  (or (gethash symbol *obfuscation-table*)
      (setf (gethash symbol *obfuscation-table*) (string (gensym)))))

(defmethod js-translate-symbol-contextually ((symbol symbol)
					     (package ps::script-package)
					     (env ps::compilation-environment))
  (cond
    ((member (ps::script-package-lisp-package package)
	     (mapcar #'find-package '(:keyword :parenscript.global)))
     (symbol-to-js symbol))
    (*obfuscate-standard-identifiers*
     (obfuscated-symbol symbol))
    (t
     (case *package-prefix-style*
       (:prefix
;	(when (first
	(format nil "~A_~A"
		(symbol-to-js (script-package-name package))
		(symbol-to-js symbol)))
       (t
	(symbol-to-js (value symbol)))))))

(defgeneric js-translate-symbol (var)
  (:documentation "Given a JS-VARIABLE returns an output
JavaScript version of it as a string."))

(defmethod js-translate-symbol ((var js-variable))
  (js-translate-symbol (value var)))

(defmethod js-translate-symbol ((var-name symbol))
  (if parenscript::*enable-package-system*
      (js-translate-symbol-contextually
       var-name
       (ps::symbol-script-package var-name)
       ps::*compilation-environment*)
      (symbol-to-js var-name)))

(defmethod js-to-strings ((v js-variable) start-form)
  (declare (ignore start-form))
  (list (js-translate-symbol v)))

;;; arithmetic operators
(defun script-convert-op-name (op)
  (case op
    (and '\&\&)
    (or '\|\|)
    (not '!)
    (eql '\=\=)
    (=   '\=\=)
    (t op)))

(defun op-form-p (form)
  (and (listp form)
       (not (script-special-form-p form))
       (not (null (op-precedence (first form))))))

(defun klammer (string-list)
  (prepend-to-first string-list "(")
  (append-to-last string-list ")")
  string-list)

(defmethod expression-precedence ((expression expression))
  0)

(defmethod expression-precedence ((form op-form))
  (op-precedence (operator form)))

(defmethod js-to-strings ((form op-form) start-pos)
  (let* ((precedence (expression-precedence form))
	 (value-string-lists
	  (mapcar #'(lambda (x)
		      (let ((string-list (js-to-strings x (+ start-pos 2))))
			(if (>= (expression-precedence x) precedence)
			    (klammer string-list)
			    string-list)))
		  (op-args form)))
	 (max-length (- 80 start-pos 2))
	 (op-string (format nil "~A " (operator form))))
    (dwim-join value-string-lists max-length :join-before op-string)    
    ))

(defmethod js-to-strings ((one-op one-op) start-pos)
  (let* ((value (value one-op))
	 (value-strings (js-to-strings value start-pos)))
    (when (typep value 'op-form)
      (setf value-strings (klammer value-strings)))
    (if (one-op-pre-p one-op)
      (prepend-to-first value-strings
			(one-op one-op))
      (append-to-last value-strings
		      (one-op one-op)))))

;;; function calls

(defmethod js-to-strings ((form function-call) start-pos)
  (let* ((value-string-lists
	  (mapcar #'(lambda (x) (js-to-strings x (+ start-pos 2)))
		  (f-args form)))
	 (max-length (- 80 start-pos 2))
	 (args (dwim-join value-string-lists max-length
			  :start "(" :end ")" :join-after ",")))
    (etypecase (f-function form)
      (js-lambda
       (dwim-join (list (append (dwim-join (list (js-to-strings (f-function form) (+ start-pos 2)))
                                           max-length
                                           :start "(" :end ")" :separator "")
                                args))
                  max-length
                  :separator ""))
      ((or js-variable js-aref js-slot-value)
       (dwim-join (list (js-to-strings (f-function form) (+ start-pos 2))
                        args)
                  max-length
                  :separator ""))
      (function-call
       ;; TODO it adds superfluous newlines after each ()
       ;; and it's nearly the same as the js-lambda case above
       (dwim-join (list (append (dwim-join (list (js-to-strings (f-function form) (+ start-pos 2)))
                                           max-length :separator "")
                                args))
                  max-length :separator "")))))

(defmethod js-to-strings ((form method-call) start-pos)
  (let ((object (js-to-strings (m-object form) (+ start-pos 2))))
    ;; TODO: this may not be the best way to add ()'s around lambdas
    ;; probably there is or should be a more general solution working
    ;; in other situations involving lambda's
    (when (member (m-object form) (list 'js-lambda 'number-literal 'js-object 'op-form)
		  :test #'typep)  
      (push "(" object)
      (nconc object (list ")")))
    (let* ((fname (dwim-join (list object
                                   (list (js-translate-symbol (m-method form))))
                             (- 80 start-pos 2)
                             :end "("
                             :separator ""))
           (butlast (butlast fname))
           (last (car (last fname)))
           (method-and-args (dwim-join (mapcar #'(lambda (x) (js-to-strings x (+ start-pos 2)))
                                               (m-args form))
                                       (- 80 start-pos 2)
                                       :start last
                                       :end ")"
                                       :join-after ","))
           (ensure-no-newline-before-dot (concatenate 'string
                                                      (car (last butlast))
                                                      (first method-and-args))))
      (nconc (butlast butlast)
             (list ensure-no-newline-before-dot)
             (rest method-and-args)))))

;;; optimization that gets rid of nested blocks, which have no meaningful effect
;;; in javascript
(defgeneric expanded-subblocks (block)
  (:method (block)
    (list block))
  (:method ((block js-block))
    (mapcan #'expanded-subblocks (block-statements block))))

(defun consolidate-subblocks (block)
  (setf (block-statements block) (expanded-subblocks block))
  block)


(defmethod js-to-statement-strings ((body js-block) start-pos)
  (consolidate-subblocks body)
  (dwim-join (mapcar #'(lambda (x) (js-to-statement-strings x (+ start-pos 2)))
		     (block-statements body))
	     (- 80 start-pos 2)
	     :join-after ";"
	     :append-to-last #'special-append-to-last
	     :start (block-indent body) :collect nil
	     :end ";"))

(defmethod js-to-strings ((body js-block) start-pos)
  (dwim-join (mapcar #'(lambda (x) (js-to-strings x (+ start-pos 2)))
		     (block-statements body))
	     (- 80 start-pos 2)
	     :append-to-last #'special-append-to-last
	     :join-after ","
	     :start (block-indent body)))


(defmethod js-to-statement-strings ((body js-sub-block) start-pos)
  (declare (ignore start-pos))
  (nconc (list "{") (call-next-method) (list "}")))

;;; function definition
(defmethod js-to-strings ((lambda js-lambda) start-pos)
  (let ((fun-header (dwim-join (mapcar #'(lambda (x)
                                           (list (js-translate-symbol x)))
				       (lambda-args lambda))
			       (- 80 start-pos 2)
			       :start (function-start-string lambda)
			       :end ") {" :join-after ","))
	(fun-body (js-to-statement-strings (lambda-body lambda) (+ start-pos 2))))
    (nconc fun-header fun-body (list "}"))))

(defgeneric function-start-string (function)
  (:documentation "Returns the string that starts the function - this varies according to whether
this is a lambda or a defun"))

(defmethod function-start-string ((lambda js-lambda))
  "function (")

(defmethod js-to-statement-strings ((lambda js-lambda) start-pos)
  (js-to-strings lambda start-pos))

(defmethod function-start-string ((defun js-defun))
  (format nil "function ~A(" (js-translate-symbol (defun-name defun))))

;;; object creation
(defmethod js-to-strings ((object js-object) start-pos)
  (let ((value-string-lists
	 (mapcar #'(lambda (slot)
		     (let* ((slot-name (first slot))
			    (slot-string-name
			    (if (typep slot-name 'script-quote)
				(if (symbolp (value slot-name))
				    (format nil "~A" (js-translate-symbol (value slot-name)))
				    (format nil "~A" (first (js-to-strings slot-name 0))))
				(car (js-to-strings slot-name 0)))))
		       (dwim-join (list (js-to-strings (second slot) (+ start-pos 4)))
				  (- 80 start-pos 2)
				  :start (concatenate 'string slot-string-name  " : ")
				  :white-space "    ")))
		 (o-slots object)))
	(max-length (- 80 start-pos 2)))
    (dwim-join value-string-lists max-length
	       :start "{ "
	       :end " }"
	       :join-after ", "
	       :white-space "  "
	       :collect nil)))

(defmethod js-to-strings ((sv js-slot-value) start-pos)
  (append-to-last (if (typep (sv-object sv) 'js-variable)
                      (js-to-strings (sv-object sv) start-pos)
                      (list (format nil "~A" (js-to-strings (sv-object sv) start-pos))))
                  (if (typep (sv-slot sv) 'script-quote)
                      (if (symbolp (value (sv-slot sv)))
                          (format nil ".~A" (js-translate-symbol (value (sv-slot sv))))
                          (format nil ".~A" (first (js-to-strings (sv-slot sv) 0))))
                      (format nil "[~A]" (first (js-to-strings (sv-slot sv) 0))))))

;;; cond
(defmethod js-to-statement-strings ((cond js-cond) start-pos)
  (loop :for body :on (cond-bodies cond)
	:for first = (eq body (cond-bodies cond))
	:for last = (not (cdr body))
	:for test :in (cond-tests cond)
	:append (if (and last (not first) (string= (value test) "true"))
		    '("else {")
		    (dwim-join (list (js-to-strings test 0)) (- 80 start-pos 2)
			       :start (if first "if (" "else if (") :end ") {"))
	:append (js-to-statement-strings (car body) (+ start-pos 2))
	:collect "}"))

(defmethod js-to-statement-strings ((if js-if) start-pos)
  (let ((if-strings (dwim-join (list (js-to-strings (if-test if) 0))
			       (- 80 start-pos 2)
			       :start "if ("
			       :end ") {"))
	(then-strings (js-to-statement-strings (if-then if) (+ start-pos 2)))
	(else-strings (when (if-else if)
			(js-to-statement-strings (if-else if)
						 (+ start-pos 2)))))
    (nconc if-strings then-strings (if else-strings
				       (nconc (list "} else {") else-strings (list "}"))
				       (list "}")))))

(defmethod js-to-strings ((if js-if) start-pos)
  (assert (typep (if-then if) 'expression))
  (when (if-else if)
    (assert (typep (if-else if) 'expression)))
  (dwim-join (list (append-to-last (js-to-strings (if-test if) start-pos) " ?")
		   (let* ((new-then (make-instance 'js-block
						   :statements (block-statements (if-then if))
						   :indent ""))
			  (res (js-to-strings new-then start-pos)))
		     (if (>= (expression-precedence (if-then if))
			     (expression-precedence if))
			     (klammer res)
			     res))
		   (list ":")
		   (if (if-else if)
		       (let* ((new-else (make-instance 'js-block
						       :statements (block-statements (if-else if))
						       :indent ""))
			      (res (js-to-strings new-else start-pos)))
			 (if (>= (expression-precedence (if-else if))
				 (expression-precedence if))
			     (klammer res)
			     res))
		       (list "undefined")))
	     (- 80 start-pos 2)
	     :white-space "  "))

;;; setf
(defmethod js-to-strings ((setf js-setf) start-pos)
  (dwim-join (cons (js-to-strings (setf-lhs setf) start-pos)
		   (mapcar #'(lambda (x) (js-to-strings x start-pos)) (setf-rhsides setf)))
	     (- 80 start-pos 2)
	     :join-after " ="))

;;; defvar
(defmethod js-to-statement-strings ((defvar js-defvar) start-pos)
  (dwim-join (nconc (mapcar #'(lambda (x) (list (js-translate-symbol x))) (var-names defvar))
		    (when (var-value defvar)
		      (list (js-to-strings (var-value defvar) start-pos))))
	     (- 80 start-pos 2)
	     :join-after " ="
	     :start "var " :end ";"))

;;; iteration
(defmethod js-to-statement-strings ((for js-for) start-pos)
  (let* ((init (dwim-join (mapcar #'(lambda (x)
				      (dwim-join (list (list (js-translate-symbol (first (var-names x))))
						       (js-to-strings (var-value x)
								      (+ start-pos 2)))
						 (- 80 start-pos 2)
						 :join-after " ="))
				  (for-vars for))
			  (- 80 start-pos 2)
			  :start "var " :join-after ","))
	 (check (js-to-strings (for-check for) (+ start-pos 2)))
	 (steps (dwim-join (mapcar #'(lambda (x var)
				       (dwim-join
					(list (list (js-translate-symbol (first (var-names var))))
					      (js-to-strings x (- start-pos 2)))
					(- 80 start-pos 2)
					:join-after " ="))
				   (for-steps for)
				   (for-vars for))
			   (- 80 start-pos 2)
			   :join-after ","))
	 (header (dwim-join (list init check steps)
			    (- 80 start-pos 2)
			    :start "for (" :end ") {"
			    :join-after ";"))
	 (body (js-to-statement-strings (for-body for) (+ start-pos 2))))
    (nconc header body (list "}"))))


(defmethod js-to-statement-strings ((fe for-each) start-pos)
  (let ((header (dwim-join (list (list (js-translate-symbol (fe-name fe)))
				 (list "in")
				 (js-to-strings (fe-value fe) (+ start-pos 2)))
			   (- 80 start-pos 2)
			   :start "for (var "
			   :end ") {"))
	(body (js-to-statement-strings (fe-body fe) (+ start-pos 2))))
    (nconc header body (list "}"))))

(defmethod js-to-statement-strings ((while js-while) start-pos)
  (let ((header (dwim-join (list (js-to-strings (while-check while) (+ start-pos 2)))
			   (- 80 start-pos 2)
			   :start "while ("
			   :end ") {"))
	(body (js-to-statement-strings (while-body while) (+ start-pos 2))))
    (nconc header body (list "}"))))

;;; with
(defmethod js-to-statement-strings ((with js-with) start-pos)
  (nconc (dwim-join (list (js-to-strings (with-obj with) (+ start-pos 2)))
		    (- 80 start-pos 2)
		    :start "with (" :end ") {")
	 (js-to-statement-strings (with-body with) (+ start-pos 2))
	 (list "}")))

;;; switch
(defmethod js-to-statement-strings ((case js-switch) start-pos)
  (let ((body 	 (mapcan #'(lambda (clause)
		     (let ((val (car clause))
			   (body (second clause)))
		       (dwim-join (list (if (eql val 'default)
					    (list "")
					    (js-to-strings val (+ start-pos 2)))
					(js-to-statement-strings body (+ start-pos 2)))
				  (- 80 start-pos 2)
				  :start (if (eql val 'default) "  default" "  case ")
				  :white-space "   "
				  :join-after ":"))) (case-clauses case))))

    #+nil
    (format t "body: ~S~%" body)
    (nconc (dwim-join (list (js-to-strings (case-value case) (+ start-pos 2)))
		    (- 80 start-pos 2)
		    :start "switch (" :end ") {")
	   body
	   (list "}"))))

;;; try-catch
(defmethod js-to-statement-strings ((try js-try) start-pos)
  (let* ((catch (try-catch try))
	 (finally (try-finally try))
	 (catch-list (when catch
		       (nconc
			(dwim-join (list (list (js-translate-symbol (first catch))))
				   (- 80 start-pos 2)
				   :start "} catch ("
				   :end ") {")
			(js-to-statement-strings (second catch) (+ start-pos 2)))))
	 (finally-list (when finally
			 (nconc (list "} finally {")
				(js-to-statement-strings finally (+ start-pos 2))))))
    (nconc (list "try {")
	   (js-to-statement-strings (try-body try) (+ start-pos 2))
	   catch-list
	   finally-list
	   (list "}"))))

;;; regex
(defun first-slash-p (string)
  (and (> (length string) 0)
       (eq (char string 0) '#\/)))

(defmethod js-to-strings ((regex regex) start-pos)
   (declare (ignore start-pos))
   (let ((slash (if (first-slash-p (value regex)) nil "/")))
     (list (format nil (concatenate 'string slash "~A" slash) (value regex)))))

;;; conditional compilation
(defmethod js-to-statement-strings ((cc cc-if) start-pos)
  (nconc (list (format nil "/*@if ~A" (cc-if-test cc)))
	 (mapcan #'(lambda (x) (js-to-strings x start-pos)) (cc-if-body cc))
	 (list "@end @*/")))


;;; TODO instanceof
(defmethod js-to-strings ((instanceof js-instanceof) start-pos)
  (dwim-join
   (list (js-to-strings (value instanceof) (+ start-pos 2))
         (list "instanceof")
         (js-to-strings (slot-value instanceof 'type) (+ start-pos 2)))
   (- 80 start-pos 2)
   :start "("
   :end ")"
   :white-space
   "  "))

;;; single operations
(defmacro define-translate-js-single-op (name &optional (superclass 'expression))
    (let ((script-name (intern (concatenate 'string "JS-" (symbol-name name)) #.*package*)))
      `(defmethod ,(if (eql superclass 'expression)
                       'js-to-strings
		       'js-to-statement-strings)
	((,name ,script-name) start-pos)
	(dwim-join (list (js-to-strings (value ,name) (+ start-pos 2)))
	 (- 80 start-pos 2)
	 :start ,(concatenate 'string (string-downcase (symbol-name name)) " ")
	 :white-space "  "))))

(define-translate-js-single-op return statement)
(define-translate-js-single-op throw statement)
(define-translate-js-single-op delete)
(define-translate-js-single-op void)
(define-translate-js-single-op typeof)
(define-translate-js-single-op new)