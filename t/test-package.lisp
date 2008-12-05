(in-package :cl-user)

(defpackage :parenscript-test
  (:nicknames :ps-test :ps-tests :parenscript-tests)
  (:use :common-lisp :parenscript :5am)
  (:shadowing-import-from :js :!)
  (:export #:run-tests
           #:make-reference-tests-dot-lisp))
