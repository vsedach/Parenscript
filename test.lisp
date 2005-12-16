(in-package :js-test)
;; Testcases for parenscript

(defun trim-whitespace(str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defmacro test-ps-js (testname parenscript javascript)
  `(test ,testname ()
    (is (string= (trim-whitespace (js-to-string ',parenscript))
                 (trim-whitespace ,javascript)))))

(defun run-tests()
  (run! 'ref-tests))
