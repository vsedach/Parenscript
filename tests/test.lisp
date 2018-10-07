;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:parenscript.tests)

(defun normalize-js-output (str)
  (cl-ppcre:regex-replace-all "\\s+" str " "))

(defmacro test-ps-js (testname parenscript javascript
                      &key (js-target-version *js-target-version*))
  `(test ,testname ()
         (is (string= (normalize-js-output ,javascript)
                      (normalize-js-output
                       (let ((*js-target-version* ,js-target-version))
                         (ps-doc* ',parenscript)))))))

(defun jsarray (contents)
  (cl-js:js-array
   (make-array (length contents)
               :initial-contents (mapcar (lambda (x)
                                           (if (listp x)
                                               (jsarray x)
                                               x))
                                         contents)
               :adjustable t)))

(defmacro %test-js-eval (testname parenscript test-statement)
  `(test ,testname ()
     (cl-js:with-js-env ()
       (let ((js-result (cl-js:run-js (ps-doc* ',parenscript))))
         ,test-statement))))

(defmacro test-js-eval (testname parenscript result)
  `(%test-js-eval ,testname ,parenscript
     (is (equalp js-result
                 ,(if (atom result)
                      result
                      `(jsarray ,result))))))

(defmacro test-js-eval-epsilon (testname parenscript result)
  `(%test-js-eval ,testname ,parenscript
     (is (< (abs (- js-result ,result)) 0.0001))))

(def-suite parenscript-tests)
(def-suite output-tests :in parenscript-tests)
(def-suite package-system-tests :in parenscript-tests)
(def-suite eval-tests :in parenscript-tests)

(defun run-tests()
  (format t "Running output tests:~&")
  (run! 'output-tests)
  (format t "Running package system tests:~&")
  (run! 'package-system-tests)
  (format t "Running CL-JavaScript eval tests:~&")
  (run! 'eval-tests))
