(in-package #:cl)
(named-readtables:in-readtable :parenscript)

(defpackage #:parenscript.tests
  (:use #:cl #:parenscript #:fiveam)
  (:export
   #:parenscript-tests
   #:run-tests
   #:interface-function
   #:test-js-eval
   #:test-js-eval-epsilon
   #:jsarray))

(defpackage #:parenscript.eval-tests
  (:use #:cl #:fiveam #:parenscript #:parenscript.tests))
