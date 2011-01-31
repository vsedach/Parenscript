;;;; -*- lisp -*-

(defsystem :parenscript.test
  :components ((:module :t
                        :serial t
                        :components ((:file "test-package")
                                     (:file "test")
                                     (:file "output-tests")
                                     (:file "package-system-tests"))))
  :depends-on (:parenscript :eos))
