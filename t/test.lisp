(in-package :js-test)

;; Testcases for parenscript

(defun normalize-whitespace (str)
  (substitute #\Space #\Newline (substitute #\Space #\Tab str)))

(defun same-space-between-statements(code)
  (cl-ppcre:regex-replace-all "\\s*;\\s*" code "; "))

(defun remove-duplicate-spaces (str)
  (labels ((spacep (char) (and char (char= char #\Space)))
           (rds (list)
             (cond ((null list) nil)
                   ((and (spacep (first list)) (spacep (second list))) (rds (cons #\Space (cddr list))))
                   (t (cons (car list) (rds (cdr list)))))))
    (coerce (rds (coerce str 'list)) 'string)))

(defun trim-spaces (str)
  (string-trim '(#\Space) str))

(defun remove-spaces-near-brackets (str)
  (reduce (lambda (str rex-pair) (cl-ppcre:regex-replace-all (first rex-pair) str (second rex-pair)))
          (cons str '(("\\[ " "[") (" \\]" "]") ("\\( " "(") (" \\)" ")")))))

(defun normalize-js-code (str)
  (remove-spaces-near-brackets
   (trim-spaces
    (remove-duplicate-spaces
     (same-space-between-statements
      (normalize-whitespace str))))))

(defmacro test-ps-js (testname parenscript javascript)
  `(test ,testname ()
    (setf js::*var-counter* 0)
    ;; is-macro expands its argument again when reporting failures, so
    ;; the reported temporary js-variables get wrong if we don't evalute first.
    (let ((generated-code (compile-script ',parenscript))
          (js-code ,javascript))
      (is (string= (normalize-js-code generated-code)
                   (normalize-js-code js-code))))))

(defun run-tests()
  (format t "Running reference tests:~&")
  (run! 'ref-tests)
  (format t "Running other tests:~&")
  (run! 'ps-tests))

