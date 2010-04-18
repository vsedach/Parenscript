(in-package #:cl)

(defpackage #:parenscript-test
  (:nicknames #:ps-test)
  (:use #:common-lisp #:parenscript #:5am)
  (:export #:run-tests #:interface-function))
