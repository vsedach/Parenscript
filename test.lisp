(in-package :js-test)

;; Testcases for parenscript

(defun trim-whitespace(str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun same-space-between-statements(code)
  (cl-ppcre:regex-replace-all "\\s*;\\s*" code (concatenate 'string (list #\; #\Newline))))

(defun no-indentation(code)
  (cl-ppcre:regex-replace-all (cl-ppcre:create-scanner "^\\s*" :multi-line-mode t) code ""))

(defun no-trailing-spaces(code)
  (cl-ppcre:regex-replace-all (cl-ppcre:create-scanner "\\s*$" :multi-line-mode t) code ""))

(defun normalize-js-code(str)
  (trim-whitespace (no-indentation (no-trailing-spaces (same-space-between-statements str)))))

(defmacro test-ps-js (testname parenscript javascript)
  `(test ,testname ()
    (setf js::*var-counter* 0)
    ;; is-macro expands its argument again when reporting failures, so
    ;; the reported temporary js-variables get wrong if we don't evalute first.
    (let ((generated-code (js-to-string ',parenscript))
          (js-code ,javascript))
      (is (string= (normalize-js-code generated-code)
                   (normalize-js-code js-code)))))) 

(defun run-tests()
  (format t "Running reference tests:~&")
  (run! 'ref-tests)
  (format t "Running other tests:~&")
  (run! 'ps-tests))

;;---------------------------------------------------------------------------
(def-suite ps-tests)
(in-suite ps-tests)

;; A problem with long nested operator, when the statement spanned several rows
;; the rows would not be joined together correctly.
(test-ps-js bug-dwim-join
   (alert (html ((:div :id 777
                       :style (css-inline :border "1pxsssssssssss"
                                          :font-size "x-small"
                                          :height (* 2 200)
                                          :width (* 2 300))))))
   "alert
('<div id=\"777\" style=\"'
 + ('border:1pxsssssssssss;font-size:x-small;height:' + 2 * 200 + ';width:'
 + 2 * 300)
 + '\"></div>')") ;";This line should start with a plus character.
