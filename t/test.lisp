(in-package :ps-test)

(defun normalize-whitespace (str)
  (substitute #\Space #\Newline (substitute #\Space #\Tab str)))
    
(defun same-space-between-statements(code)
  (let ((cl-ppcre:*use-bmh-matchers* nil)) ;; disable Booyer Moore string matching algorithm, which doesn't work very well on unicode lisps
    (cl-ppcre:regex-replace-all "\\s*;\\s*" code "; ")))
    
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
  (let ((cl-ppcre:*use-bmh-matchers* nil)) ;; disable Booyer Moore string matching algorithm, which doesn't work very well on unicode lisps
    (reduce (lambda (str rex-pair) (cl-ppcre:regex-replace-all (first rex-pair) str (second rex-pair)))
            (cons str '(("\\[ " "[") (" \\]" "]") ("\\( " "(") (" \\)" ")"))))))
  
(defun normalize-js-code (str)
  (remove-spaces-near-brackets
   (trim-spaces
    (remove-duplicate-spaces
     (same-space-between-statements
      (normalize-whitespace str))))))

(defmacro test-ps-js (testname parenscript javascript)
  (let (
        ;; (parenscript
        ;;   `(progn
        ;; (defpackage parenscript-test
        ;; (:lisp-package :parenscript-test))
        ;; ,parenscript)))
        )
    `(test ,testname ()
      (setf ps:*ps-gensym-counter* 0)
    
      ;; is-macro expands its argument again when reporting failures, so
      ;; the reported temporary js-variables get wrong if we don't evalute first.
      (let* ((generated-code (ps1* ',parenscript))
             (js-code ,javascript))
        (is (string= (normalize-js-code generated-code)
                     (normalize-js-code js-code)))))))

(defmacro defpstest (testname (&key (optimize t)) parenscript javascript)
  (declare (ignore optimize))
  `(test ,testname
    (setf ps:*ps-gensym-counter* 0)
    (let* ((generated-code (ps1* ',parenscript))
           (js-code ,javascript))
      (is (string= (normalize-js-code generated-code)
                   (normalize-js-code js-code))))))

(defun run-tests()
  (format t "Running reference tests:~&")
  (run! 'ref-tests)
  (format t "Running other tests:~&")
  (run! 'ps-tests)
  (format t "Running Package System tests:~&")
  (run! 'package-system-tests))

