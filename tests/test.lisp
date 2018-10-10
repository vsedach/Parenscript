;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:parenscript.tests)

(defun normalize-js-output (str)
  (cl-ppcre:regex-replace-all "\\s+" str " "))

(defmacro test-ps-js (testname parenscript javascript
                      &key (js-target-version *js-target-version*))
  `(fiveam:test ,testname ()
     (fiveam:is
      (string= (normalize-js-output ,javascript)
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
  `(fiveam:test ,testname ()
     (cl-js:with-js-env ()
       (let ((js-result (cl-js:run-js (ps-doc* ',parenscript))))
         ,test-statement))))

(defmacro test-js-eval (testname parenscript result)
  `(%test-js-eval ,testname ,parenscript
     (fiveam:is (equalp js-result
                        ,(if (atom result)
                             result
                             `(jsarray ,result))))))

(defmacro test-js-eval-epsilon (testname parenscript result)
  `(%test-js-eval ,testname ,parenscript
     (fiveam:is (< (abs (- js-result ,result)) 0.0001))))

(fiveam:def-suite parenscript-tests)
(fiveam:def-suite output-tests         :in parenscript-tests)
(fiveam:def-suite package-system-tests :in parenscript-tests)
(fiveam:def-suite eval-tests           :in parenscript-tests)

(defun run-tests ()
  (fiveam:run! 'parenscript-tests))
