;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ParenScript namespace system

(in-package :parenscript)

(defun lisp-symbol-to-ps-identifier (symbol context)
  (case context
    (:special-form (symbol-name symbol))
    (:macro symbol)
    (otherwise (symbol-name symbol))))

;;; Symbol obfuscation
(defvar *obfuscation-table* (make-hash-table))

(defun obfuscate-package (package-designator)
  (setf (gethash (find-package package-designator) *obfuscation-table*) (make-hash-table)))

(defun unobfuscate-package (package-designator)
  (remhash (find-package package-designator) *obfuscation-table*))

(defun maybe-obfuscate-symbol (symbol)
  (let ((obfuscated-symbol-table (gethash (symbol-package symbol) *obfuscation-table*)))
    (if obfuscated-symbol-table
        (or (gethash symbol obfuscated-symbol-table)
            (setf (gethash symbol obfuscated-symbol-table) (ps-gensym "G")))
        symbol)))

;;; Interface for printing identifiers

(defvar *package-prefix-table* (make-hash-table))

(defmacro ps-package-prefix (package)
  "Place for storing a string to be prefixed to any symbols in the
designated package when translating ParenScript code."
  `(gethash (find-package ,package) *package-prefix-table*))

(defun js-translate-symbol (symbol)
  (let ((possibly-obfuscated-symbol (maybe-obfuscate-symbol symbol)))
    (if (ps-package-prefix (symbol-package symbol))
        (format nil "~A~A" (ps-package-prefix (symbol-package symbol)) (symbol-to-js possibly-obfuscated-symbol))
        (symbol-to-js possibly-obfuscated-symbol))))
