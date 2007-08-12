;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ParenScript namespace system

(in-package :parenscript)

(defun lisp-symbol-to-ps-identifier (symbol context)
  (case context
    (:special-form (symbol-name symbol))
    (:macro symbol)
    (otherwise (symbol-name symbol))))

;;; Symbol obfuscation
(defvar *obfuscate-identifiers* nil)

(defparameter *obfuscation-table* (make-hash-table))

(defun obfuscated-symbol (symbol)
  (or (gethash symbol *obfuscation-table*)
      (setf (gethash symbol *obfuscation-table*) (string (gensym)))))

;;; Interface for printing identifiers

(defvar *package-prefix-style* :prefix
  "Determines how package symbols are serialized to JavaScript identifiers.  NIL for
no prefixes.  :prefix to prefix variables with something like packagename_identifier.")

(defvar *package-prefix-table* (make-hash-table))

(defmacro ps-package-prefix (package)
  "Place for storing a string to be prefixed to any symbols in the
designated package when translating ParenScript code."
  `(gethash (find-package ,package) *package-prefix-table*))

(defun js-translate-symbol (symbol)
  (cond (*obfuscate-identifiers* (obfuscated-symbol symbol))
        ((and (eql *package-prefix-style* :prefix) (ps-package-prefix (symbol-package symbol)))
         (format nil "~A~A" (ps-package-prefix (symbol-package symbol)) (symbol-to-js symbol)))
        (t (symbol-to-js symbol))))

