(in-package #:parenscript)
(in-readtable :parenscript)

(defvar *obfuscated-packages* (make-hash-table))

(defun obfuscate-package (package-designator &optional
                          (symbol-map
                           (let ((symbol-table (make-hash-table)))
                             (lambda (symbol)
                               (or #1=(gethash symbol symbol-table)
                                   (setf #1# (ps-gensym "G")))))))
  (setf (gethash (find-package package-designator) *obfuscated-packages*) symbol-map))

(defun unobfuscate-package (package-designator)
  (remhash (find-package package-designator) *obfuscated-packages*))

(defun maybe-obfuscate-symbol (symbol)
  (if (aand (symbol-package symbol) (eq :external (nth-value 1 (find-symbol (symbol-name symbol) it))))
      symbol
      (aif (gethash (symbol-package symbol) *obfuscated-packages*)
           (funcall it symbol)
           symbol)))

(defvar *package-prefix-table* (make-hash-table))

(defmacro ps-package-prefix (package)
  `(gethash (find-package ,package) *package-prefix-table*))

(defun symbol-to-js-string (symbol &optional (mangle-symbol-name? t))
  (let* ((symbol-name (symbol-name (maybe-obfuscate-symbol symbol)))
         (identifier (if mangle-symbol-name?
                         (encode-js-identifier symbol-name)
                         symbol-name)))
    (aif (ps-package-prefix (symbol-package symbol))
         (concatenate 'string it identifier)
         identifier)))
