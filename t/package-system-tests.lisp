(in-package :parenscript-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite package-system-tests))

(in-suite package-system-tests)

(defpstest simple-variable-prefix ()
  (progn
    (defpackage test (:lisp-package :parenscript-test))
    (defvar the-var))
   "var test_theVar;")

(defpstest no-global-variable-prefix ()
  (progn
    (defvar global::the-var)
    (defvar global::global))
   "var theVar; var global;")

(defpstest eval-when-returns-paren-form ()
  (progn
    (eval-when (:compile-toplevel)
      `(global::sort-of-macro-like))
    global::treat-me-right)
  "sortOfMacroLike(); treatMeRight;")

(defpstest javascript-operations ()
  (progn
    (+ 1 2 3 4)
    (- 1 2 3 4)
    nil t this false undefined)
  "1 + 2 + 3 + 4; 1 - 2 - 3 - 4; null; true; this; false; undefined;")

(defpstest def-keywords ()
  (progn
    (defun global::hello-world () (return 5)))
  "function helloWorld() { return 5; };")

(defpstest ps-js-reserved ()
  (eval-when (:compile-toplevel)
    (format nil "~A" (script-package-name (symbol-script-package 'defclass))))
  "'JAVASCRIPT';")

