(in-package :cl-user)

(defpackage :parenscript-test
  (:nicknames :js-test)
  (:use :common-lisp :js :5am)
  (:shadowing-import-from :js :!)
  (:export #:run-tests
           #:make-reference-tests-dot-lisp))
