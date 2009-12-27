(in-package #:cl)

(defpackage #:parenscript-test
  (:nicknames #:ps-test)
  (:use #:common-lisp #:parenscript #:5am)
  (:shadowing-import-from #:js #:!)
  (:export #:run-tests))
