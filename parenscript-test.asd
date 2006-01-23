;;;; -*- lisp -*-

(in-package :cl-user)

(defpackage #:parenscript-test.system
  (:use :cl :asdf))

(in-package #:parenscript-test.system)

(defsystem #:parenscript-test
  :depends-on (:parenscript :fiveam :cl-ppcre)
  :components ((:module :t
                :components ((:file "test-package")
                             (:file "test" :depends-on ("test-package"))
                             (:file "ref2test" :depends-on ("test"))
                             (:file "reference-tests" :depends-on ("test"))))))
