(in-package parenscript)
;;;; This software was taken from the SBCL system.  there are very few 
;;;; changes, and one SBCL-specific thing left (sb-c::collect

;;; if you have found this on google, THIS IS NOT AN SBCL SOURCE FILE

;;; Break something like a lambda list (but not necessarily actually a
;;; lambda list, e.g. the representation of argument types which is
;;; used within an FTYPE specification) into its component parts. We
;;; return twelve values:
;;;  1. a list of the required args;
;;;  2. a list of the &OPTIONAL arg specs;
;;;  3. true if a &REST arg was specified;
;;;  4. the &REST arg;
;;;  5. true if &KEY args are present;
;;;  6. a list of the &KEY arg specs;
;;;  7. true if &ALLOW-OTHER-KEYS was specified.;
;;;  8. true if any &AUX is present (new in SBCL vs. CMU CL);
;;;  9. a list of the &AUX specifiers;
;;; 10. true if a &MORE arg was specified;
;;; 11. the &MORE context var;
;;; 12. the &MORE count var;
;;; 13. true if any lambda list keyword is present (only for
;;;     PARSE-LAMBDA-LIST-LIKE-THING).
;;;
;;; The top level lambda list syntax is checked for validity, but the
;;; arg specifiers are just passed through untouched. If something is
;;; wrong, we use COMPILER-ERROR, aborting compilation to the last
;;; recovery point.
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun collect-list-expander (n-value n-tail forms)
    (let ((n-res (gensym)))
      `(progn
	,@(mapcar (lambda (form)
		    `(let ((,n-res (cons ,form nil)))
		      (cond (,n-tail
			     (setf (cdr ,n-tail) ,n-res)
			     (setq ,n-tail ,n-res))
			    (t
			     (setq ,n-tail ,n-res  ,n-value ,n-res)))))
		  forms)
	,n-value))))
  
(defmacro collect (collections &body body)
  (let ((macros ())
	(binds ()))
    (dolist (spec collections)
					;      (unless (proper-list-of-length-p spec 1 3)
					;        (error "malformed collection specifier: ~S" spec))
      (let* ((name (first spec))
	     (default (second spec))
	     (kind (or (third spec) 'collect))
	     (n-value (gensym (concatenate 'string
					   (symbol-name name)
					   "-N-VALUE-"))))
	(push `(,n-value ,default) binds)
	(if (eq kind 'collect)
	    (let ((n-tail (gensym (concatenate 'string
					       (symbol-name name)
					       "-N-TAIL-"))))
	      (if default
		  (push `(,n-tail (last ,n-value)) binds)
		  (push n-tail binds))
		(push `(,name (&rest args)
			(collect-list-expander ',n-value ',n-tail args))
		      macros))
	    (push `(,name (&rest args)
		    (collect-normal-expander ',n-value ',kind args))
		  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))
  
(defparameter *lambda-list-keywords*
 '(&allow-other-keys &aux &body &environment &key &optional &rest &whole))

(defun style-warn (&rest args) (apply #'format t args))


(defun parse-lambda-list-like-thing (list)
 (collect ((required)
            (optional)
            (keys)
            (aux))
    (let ((restp nil)
          (rest nil)
          (morep nil)
          (more-context nil)
          (more-count nil)
          (keyp nil)
          (auxp nil)
          (allowp nil)
          (state :required))
      (declare (type (member :allow-other-keys :aux
                             :key
                             :more-context :more-count
                             :optional
                             :post-more :post-rest
                             :required :rest)
                     state))
      (dolist (arg list)
        (if (member arg *lambda-list-keywords*)
            (case arg
              (&optional
               (unless (eq state :required)
                 (format t "misplaced &OPTIONAL in lambda list: ~S"
                                 list))
               (setq state :optional))
              (&rest
               (unless (member state '(:required :optional))
                 (format t "misplaced &REST in lambda list: ~S" list))
               (setq state :rest))
              (&more
               (unless (member state '(:required :optional))
                 (format t "misplaced &MORE in lambda list: ~S" list))
               (setq morep t
                     state :more-context))
              (&key
               (unless (member state
                               '(:required :optional :post-rest :post-more))
                 (format t "misplaced &KEY in lambda list: ~S" list))
               #-sb-xc-host
               (when (optional)
                 (format t
                  "&OPTIONAL and &KEY found in the same lambda list: ~S" list))
               (setq keyp t
                     state :key))
              (&allow-other-keys
               (unless (eq state ':key)
                 (format t "misplaced &ALLOW-OTHER-KEYS in ~
                                  lambda list: ~S"
                                 list))
               (setq allowp t
                     state :allow-other-keys))
              (&aux
               (when (member state '(:rest :more-context :more-count))
                 (format t "misplaced &AUX in lambda list: ~S" list))
               (when auxp
                 (format t "multiple &AUX in lambda list: ~S" list))
               (setq auxp t
                     state :aux))
              (t (format t "unknown LAMBDA-LIST-KEYWORD in lambda list: ~S." arg)))
            (progn
              (when (symbolp arg)
                (let ((name (symbol-name arg)))
                  (when (and (plusp (length name))
                             (char= (char name 0) #\&))
                    (style-warn
                     "suspicious variable in lambda list: ~S." arg))))
              (case state
                (:required (required arg))
                (:optional (optional arg))
                (:rest
                 (setq restp t
                       rest arg
                       state :post-rest))
                (:more-context
                 (setq more-context arg
                       state :more-count))
                (:more-count
                 (setq more-count arg
                       state :post-more))
                (:key (keys arg))
                (:aux (aux arg))
                (t
                 (format t "found garbage in lambda list when expecting ~
                                  a keyword: ~S"
                                 arg))))))
      (when (eq state :rest)
        (format t "&REST without rest variable"))

      (values (required) (optional) restp rest keyp (keys) allowp auxp (aux)
              morep more-context more-count
              (not (eq state :required))))))

;;; like PARSE-LAMBDA-LIST-LIKE-THING, except our LAMBDA-LIST argument
;;; really *is* a lambda list, not just a "lambda-list-like thing", so
;;; can barf on things which're illegal as arguments in lambda lists
;;; even if they could conceivably be legal in not-quite-a-lambda-list
;;; weirdosities
(defun parse-lambda-list (lambda-list)

  ;; Classify parameters without checking their validity individually.
  (multiple-value-bind (required optional restp rest keyp keys allowp auxp aux
                        morep more-context more-count)
      (parse-lambda-list-like-thing lambda-list)

    ;; Check validity of parameters.
    (flet ((need-symbol (x why)
             (unless (or (symbolp x) t)
               (format t "~A is not a symbol: ~S" why x))))
      (dolist (i required)
        (need-symbol i "Required argument"))
      (dolist (i optional)
        (typecase i
          (symbol)
          (cons
           (destructuring-bind (var &optional init-form supplied-p) i
             (declare (ignore init-form supplied-p))
             (need-symbol var "&OPTIONAL parameter name")))
          (t
           (format t "&OPTIONAL parameter is not a symbol or cons: ~S"
                           i))))
      (when restp
        (need-symbol rest "&REST argument"))
      (when keyp
        (dolist (i keys)
          (typecase i
            (symbol)
            (cons
             (destructuring-bind (var-or-kv &optional init-form supplied-p) i
               (declare (ignore init-form supplied-p))
               (if (consp var-or-kv)
                   (destructuring-bind (keyword-name var) var-or-kv
                     (declare (ignore keyword-name))
                     (need-symbol var "&KEY parameter name"))
                   (need-symbol var-or-kv "&KEY parameter name"))))
            (t
             (format t "&KEY parameter is not a symbol or cons: ~S"
                             i))))))

    ;; Voila.
    (values required optional restp rest keyp keys allowp auxp aux
            morep more-context more-count)))
