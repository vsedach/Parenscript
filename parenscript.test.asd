;;;; -*- lisp -*-

(defsystem :parenscript.test
  :license "Public Domain"
  :components ((:module :t
                        :serial t
                        :components ((:file "test-package")
                                     (:file "test")
                                     (:file "output-tests")
                                     (:file "package-system-tests")
                                     (:file "eval-tests"))))
  :depends-on (:parenscript :eos :cl-js))
