(in-package #:cl)
(named-readtables:in-readtable :parenscript)

(defpackage #:ps-test
  (:use #:cl #:parenscript #:eos)
  (:export #:parenscript-tests #:run-tests #:interface-function #:test-js-eval #:jsarray))

(defpackage #:ps-eval-tests
  (:use #:cl #:eos #:parenscript #:ps-test))
