(in-package "PARENSCRIPT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ParenScript namespace system

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

(defun symbol-to-js-string (symbol &optional (mangle-symbol-name t))
  (let ((symbol-name (funcall (if mangle-symbol-name
                                  #'symbol-name-to-js-string
                                  #'symbol-name)
                              (maybe-obfuscate-symbol symbol))))
    (aif (ps-package-prefix (symbol-package symbol))
         (format nil "~A~A" it symbol-name)
         symbol-name)))
