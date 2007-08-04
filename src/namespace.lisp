;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ParenScript package system

(in-package :parenscript)

(defclass script-package ()
  ;; configuration slots
  ((name          :accessor script-package-name          :initform nil :initarg :name :type string
		  :documentation "Canonical name of the package (a String).")
   (nicknames     :accessor script-package-nicknames     :initform nil :initarg :nicknames
		  :documentation "List of nicknames for the package (as strings).")
   (prefix        :accessor script-package-prefix        :initform nil :initarg :prefix :type string
                  :documentation "The prefix string that will be used when translating the symbols in the current package to Javascript.")
   (lisp-package  :accessor script-package-lisp-package  :initform nil :initarg :lisp-package)
   (secondary-lisp-packages :accessor script-package-secondary-lisp-packages :initform nil
			    :initarg :secondary-lisp-packages)
   (exports       :accessor script-package-exports       :initarg :exports
		  :initform nil;(make-hash-table :test #'equal)
		  :documentation "List of exported identifiers.")
   (used-packages :accessor script-package-used-packages :initform nil :initarg :used-packages
		  :documentation "")
   (documentation :accessor script-package-documentation :initform nil :initarg :documentation)
   (compilation-env :accessor script-package-comp-env    :initform nil :initarg :comp-env)
   (locked?       :accessor script-package-locked? :initform nil :initarg :locked?
		  :documentation "t if redefinition of top-level symbols is disallowed.")
   ;; internal use slots
   (symbol-table :accessor script-package-symbol-table :initform nil :initarg :symbol-table
		 :documentation "Contains symbols when there is no lisp package for this package.")
   )
  (:documentation "A Parenscript package is a lisp object that holds information
about a set of code.

"))

(defmethod print-object ((sp script-package) stream)
  (format stream "#<SCRIPT-PACKAGE ~s>" (script-package-name sp)))

(defclass compilation-environment ()
  ((script-packages :accessor comp-env-script-packages :initform nil :initarg :packages
		    :documentation "List of packages defined in this environment.")
   (current-package :accessor comp-env-current-package :initform nil :initarg :current-package
		    :documentation "Current in-package.")
   (lisp-to-script-package-table
    :accessor comp-env-lisp-to-script-package-table :initform (make-hash-table)
    :documentation   "Maps a lisp package to a script package.")
   (compiling-toplevel-p 
    :accessor comp-env-compiling-toplevel-p :initform nil :initarg :processing-toplevel-p
    :documentation "T if the environment is currently processing toplevel forms.")
   (symbol-table :accessor symbol-to-script-package :initform (make-hash-table)
		 :documentation "Maps symbols to script packages.  Used for only the
symbols in script packages that do not have a primary lisp package."))
  (:documentation ""))

(defgeneric symbol-script-package (symbol)
  (:documentation "Gets the Parenscript package associated with a Lisp/Parenscript symbol."))

(defvar *warn-ps-package* nil
  "If true, warns when ParenScript attempts to compile symbols that
don't have an associated ParenScript package.")

(defun lisp-to-script-package (lisp-package &optional (comp-env *compilation-environment*))
  "Gets a script package corresponding to the given Lisp package."
  (gethash lisp-package (comp-env-lisp-to-script-package-table comp-env)))

(defsetf lisp-to-script-package (lisp-package &optional (comp-env *compilation-environment*))
    (script-package)
  "Sets the script package corresponding to the given Lisp package."
  `(setf (gethash ,lisp-package (comp-env-lisp-to-script-package-table ,comp-env))
    ,script-package))

(defmethod symbol-script-package ((symbol symbol))
  (if (symbol-package symbol)
      (or (lisp-to-script-package (symbol-package symbol) *compilation-environment*)
          (progn (when *warn-ps-package*
                   (warn 'simple-style-warning
                         :format-control "~s is a symbol with lisp package ~s, which has no corresponding ParenScript package.
Defaulting to :parenscript-user."
                         :format-arguments (list symbol (symbol-package symbol))))
                 (find-script-package "PARENSCRIPT-USER" (make-basic-compilation-environment))))
      (find-script-package "UNINTERNED" *compilation-environment*)))

(defun find-script-package (name &optional (comp-env *compilation-environment*))
  "Find the script package with the name NAME in the given compilation environment."
  (typecase name
    ((or symbol string)
     (find-if #'(lambda (script-package)
		  (find (string name)
			(cons (script-package-name script-package)
			      (script-package-nicknames script-package))
			:test #'equal))
	      (comp-env-script-packages comp-env)))
    (script-package name)
    (t (error "~A has unknown type" name))))

(defun script-intern (name script-package-name)
  "Returns a Parenscript symbol with the string value STRING interned for the
given SCRIPT-PACKAGE."
  (declare (type string name))
  (let ((script-package (find-script-package script-package-name)))
    (flet ((find-exported-symbol (name script-package)
             (let ((res
                    (find name (script-package-exports script-package)
                          :key #'(lambda (exported-symbol) (string exported-symbol))
                          :test #'equal)))
               res)))
      (let ((res
             (or
              (some #'(lambda (used-package)
                        (find-exported-symbol name used-package))
                    (script-package-used-packages script-package))
              (if (script-package-lisp-package script-package)
                  (intern name (script-package-lisp-package script-package))
                  (progn
                    (let ((sym (intern-without-package name)))
                      (setf (gethash name (script-package-symbol-table script-package))
                            sym)
                      (setf (gethash sym (symbol-to-script-package (script-package-comp-env script-package)))
                            script-package)
                      sym))))))
        (declare (type symbol res))
        res))))

(defun find-script-symbol (name script-package)
  "Finds the symbol with name NAME in the script package SCRIPT-PACKAGE.  NAME is a
string and SCRIPT-PACKAGE is a package designator.  If NAME does not specify a symbol of
script-package, returns nil.  Otherwise returns 2 values:
1.  the symbol
2.  :external if the symbol is external.  :internal if the symbol is internal. NIL if
the symbol is not interned in the package."
  (setf script-package (find-script-package script-package))
  (let (symbol interned-p)

    (if (script-package-lisp-package script-package)
	(multiple-value-bind (lisp-symbol lisp-status)
	    (find-symbol name (script-package-lisp-package script-package))
	  (setf symbol lisp-symbol)
	  (setf interned-p (and lisp-status t)))
	(multiple-value-bind (sym sym-found-p)
	    (gethash name (script-package-symbol-table script-package))
	  (setf symbol sym)
	  (setf interned-p sym-found-p)))
    (let ((exported? (member symbol (script-package-exports script-package))))
      (values symbol
	      (if exported? :external (if interned-p :internal nil))))))

(defun script-export (symbols
		      &optional (script-package (comp-env-current-package *compilation-environment*)))
  "Exports the given symbols in the given script package."
  (when (not (listp symbols)) (setf symbols (list symbols)))
  (setf script-package (find-script-package script-package))
  (let ((symbols-not-in-package
	 (remove-if #'(lambda (symbol)
			(declare (type symbol symbol))
			(eql symbol (find-script-symbol (string symbol) script-package)))
		    symbols)))
    (when symbols-not-in-package
      (error "Invalid exports.  The following symbols are not interned in the package ~A:~%~A"
	     (script-package-name script-package) symbols-not-in-package)))
  (mapc #'(lambda (symbol)
	    (pushnew symbol (script-package-exports script-package)))
	symbols)
  t)

(defun use-script-package (packages-to-use
			   &optional (into-package (comp-env-current-package *compilation-environment*)))
  "use-script-package causes INTO-PACKAGE to inherit all the external symbols of packages-to-use. 
The inherited symbols become accessible as internal symbols of package."
  (when (not (listp packages-to-use)) (setf packages-to-use (list packages-to-use)))
  (setf packages-to-use (mapcar #'find-script-package packages-to-use))
  (setf into-package (find-script-package into-package))

  (let ((all-used-symbols (apply #'append (mapcar #'script-package-exports packages-to-use))))
    (mapc #'(lambda (used-symbol)
	      (let ((symbol-same-name (find-script-symbol (string used-symbol) into-package)))
		(when (not (or (null symbol-same-name)
			       (eql symbol-same-name used-symbol)))
		  (error "Import of symbol ~A into package ~A conflicts with interned symbol ~A"
			 used-symbol (script-package-name into-package) symbol-same-name))))
	  all-used-symbols))
  (setf (script-package-used-packages into-package)
	(append (script-package-used-packages into-package) packages-to-use)))

(defun intern-without-package (name)
  (macrolet ((with-temp-package ((var) &body body)
	       (let ((result-var (gensym)))
	       `(let* ((,var (make-package ',(gensym)))
		       (,result-var (progn ,@body)))
		 (delete-package ,var)
		 ,result-var))))
    (with-temp-package (package)
      (let ((sym (intern name package)))
	(unintern sym package)
	sym))))

(defun create-script-package (comp-env
			      &key name nicknames prefix secondary-lisp-packages used-packages
			      lisp-package exports documentation)
  "Creates a script package in the given compilation environment"
  (when (and lisp-package (not (find-package lisp-package)))
    (error "Package ~A does not exists" lisp-package))
  (let* ((script-package
	  (make-instance 'script-package
			 :name (string name)
			 :comp-env comp-env
                         :prefix prefix
			 :nicknames (mapcar #'string nicknames)
			 :lisp-package (when lisp-package (find-package lisp-package))
			 :secondary-lisp-packages (mapcar #'find-package secondary-lisp-packages)
			 :documentation documentation)))
    (use-script-package used-packages script-package)
    (labels ((package-intern (string-like)
	       (script-intern (string string-like) script-package)))
      (script-export (mapcar #'package-intern exports) script-package))
    (push script-package (comp-env-script-packages comp-env))
    script-package))

(defmethod initialize-instance :after ((package script-package) &key)
  (assert (script-package-comp-env package))
  (when (null (script-package-lisp-package package))
    (setf (script-package-symbol-table package)
	  (make-hash-table :test #'equal)))
  (let ((lisp-packages
	 (remove-if #'null
		    (cons (script-package-lisp-package package)
			  (script-package-secondary-lisp-packages package)))))
    (dolist (lisp-package lisp-packages)
      (when (lisp-to-script-package lisp-package (script-package-comp-env package))
	(error "Lisp package already has corresponding script package: ~A" (package-name lisp-package)))
      (setf (lisp-to-script-package lisp-package (script-package-comp-env package))
	    package))))

(defgeneric comp-env-find-package (comp-env package-designator)
  (:documentation "Finds the script package named by PACKAGE-DESIGNATOR in the current 
compilation environment. PACKAGE-DESIGNATOR is a string or symbol.")
  (:method ((comp-env compilation-environment) (name string))
    (find name (comp-env-script-packages comp-env)
	  :key #'script-package-name :test #'equal))
  (:method ((comp-env compilation-environment) (package-designator symbol))
    (comp-env-find-package comp-env (string package-designator))))

;; TODO loop through all defined macros and add them to the script package's
;; macro environment
;	  (labels ((name-member (name)
;		     (eql (script-package-lisp-package script-package) (symbol-package name)))
;		   (import-macro (name function)
;		     (when (name-member name)
;		       (setf (gethash name (script-package-macro-table script-package))
;			     function)))
;		   (import-special-form (name function)
;		     (when (name-member name)
;		       (setf (gethash name (script-package-special-form-table script-package))
;			     function))))
;	  (maphash #'import-special-form *toplevel-special-forms*)
;	  (maphash #'import-special-form *toplevel-special-forms*)

;(defgeneric comp-env-select-package (comp-env script-package)
;  (:documentation "")
;  (:method ((comp-env compilation-environment) (package script-package))
;    (setf (comp-env-current-package 


(defvar *enable-package-system* nil)

;;; Interface for reading in identifier

(defgeneric lisp-symbol-to-ps-identifier (symbol context &optional compilation-environment) 
  (:documentation "Context is one of :special-form, :macro or nil."))

(defmethod lisp-symbol-to-ps-identifier ((symbol symbol) (context (eql :special-form)) &optional comp-ev)
  (declare (ignore context comp-ev))
  (symbol-name symbol))

(defmethod lisp-symbol-to-ps-identifier ((symbol symbol) (context (eql :macro)) &optional comp-ev)
  (declare (ignore context comp-ev))
  symbol)

(defmethod lisp-symbol-to-ps-identifier :around ((symbol symbol) context &optional comp-ev)
  (declare (ignore context comp-ev))
  (if *enable-package-system*
      (call-next-method)
      (symbol-name symbol)))

;;; Symbol obfuscation (this should really go somewhere else)
(defvar *obfuscate-standard-identifiers* nil)

(defparameter *obfuscation-table* (make-hash-table))

(defun obfuscated-symbol (symbol)
  (or (gethash symbol *obfuscation-table*)
      (setf (gethash symbol *obfuscation-table*) (string (gensym)))))

;;; Interface for printing identifiers

(defvar *package-prefix-style* :prefix
  "Determines how package symbols are serialized to JavaScript identifiers.  NIL for
no prefixes.  :prefix to prefix variables with something like packagename_identifier.")

(defgeneric js-translate-symbol-contextually (symbol package env)
  (:documentation "Translates a symbol to a string in the given environment & package
and for the given symbol."))

(defmethod js-translate-symbol-contextually ((symbol symbol) (package ps::script-package) (env ps::compilation-environment))
  (cond ((member (ps::script-package-lisp-package package) (mapcar #'find-package '(:keyword :parenscript.global)))
         (symbol-to-js symbol))
        (*obfuscate-standard-identifiers* (obfuscated-symbol symbol))
        (t (if (and *enable-package-system* (eql *package-prefix-style* :prefix))
               (format nil "~A~A"
                       (or (ps::script-package-prefix package) (concatenate 'string (ps::script-package-name package) "_"))
                       (symbol-to-js symbol))
               (symbol-to-js symbol)))))

