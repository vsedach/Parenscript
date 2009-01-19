;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ParenScript namespace system

(in-package :parenscript)

(defun ensure-ps-symbol (symbol)
  (if (eq (symbol-package symbol) #.(find-package :parenscript))
      symbol
      (intern (symbol-name symbol) #.(find-package :parenscript))))

;;; Symbol obfuscation
(defvar *obfuscated-packages* (make-hash-table))

(defun obfuscate-package (package-designator &optional (symbol-map (make-hash-table)))
  "symbol-map can either be a hash table or a closure that takes a symbol as its only argument."
  (setf (gethash (find-package package-designator) *obfuscated-packages*) symbol-map))

(defun unobfuscate-package (package-designator)
  (remhash (find-package package-designator) *obfuscated-packages*))

(defun maybe-obfuscate-symbol (symbol)
  (ctypecase (gethash (symbol-package symbol) *obfuscated-packages*)
    (hash-table (let ((symbol-map (gethash (symbol-package symbol) *obfuscated-packages*)))
                  (or (gethash symbol symbol-map) (setf (gethash symbol symbol-map) (ps-gensym "G")))))
    (function (funcall (gethash (symbol-package symbol) *obfuscated-packages*) symbol))
    (null symbol)))

;;; Interface for printing identifiers

(defvar *package-prefix-table* (make-hash-table))

(defmacro ps-package-prefix (package)
  "Place for storing a string to be prefixed to any symbols in the
designated package when translating ParenScript code."
  `(gethash (find-package ,package) *package-prefix-table*))

(defun js-translate-symbol (symbol)
  (let ((possibly-obfuscated-symbol (maybe-obfuscate-symbol symbol)))
    (if (ps-package-prefix (symbol-package symbol))
        (format nil "~A~A" (ps-package-prefix (symbol-package symbol)) (symbol-to-js-string possibly-obfuscated-symbol))
        (symbol-to-js-string possibly-obfuscated-symbol))))
