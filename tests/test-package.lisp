(in-package #:cl)
(named-readtables:in-readtable :parenscript)

(defpackage #:parenscript.tests
  (:use #:cl #:parenscript #:eos)
  (:export
   #:parenscript-tests
   #:run-tests
   #:interface-function
   #:test-js-eval
   #:test-js-eval-epsilon
   #:jsarray))

(defpackage #:parenscript.eval-tests
  (:use #:cl #:eos #:parenscript #:parenscript.tests))
