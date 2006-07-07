(in-package :js)

(defun compile-parenscript-file-to-string (source-file &key
                                                       (log-stream nil)
                                                       (comment nil)
                                                       (eval-forms-p nil))
  "Compile SOURCE-FILE (a parenscript file) to a javascript string. (in-package ...) forms
behave as expected and all other forms are evaluated according to the value of
EVAL-FORMS-P. If the result of the evaluation is not nil then it's compiled with
js:js* and written to the output."
  (with-output-to-string (output)
    (with-open-file (input source-file :direction :input)
      (flet ((read-form ()
               (read input nil))
             (log-message (&rest args)
               (when log-stream
                 (apply #'format log-stream args))))
        (let ((*package* *package*))
          (loop for form = (read-form)
                while form do
                (if (or (not (listp form))
                        (not (eq (car form) 'cl:in-package)))
                    (progn
                      (log-message "Processing form:~%~S~%" form)
                      (when comment
                        (princ "/*" output)
                        (print form output)
                        (terpri output)
                        (princ "*/" output)
                        (terpri output))
                      (when eval-forms-p
                        (setf form (eval form)))
                      (log-message "After evaluation:~%~S~%" form)
                      (when form
                        (let ((compiled (js:js* form)))
                          (log-message "Compiled into:~%~A~%~%" compiled)
                          (write-string compiled output)
                          (terpri output)
                          (terpri output))))
                    (when (and (listp form)
                               (eq (car form) 'cl:in-package))
                      (log-message "Setting package to: ~S~%" (cadr form))
                      (setf *package* (find-package (cadr form)))))))))))

(defun compile-parenscript-file (source-file &rest args &key destination-file &allow-other-keys)
  "Compile SOURCE-FILE (a parenscript file) to a javascript file with
compile-parenscript-file-to-string. When DESTINATION-FILE is omitted,
then it will be named the same as SOURCE-FILE but with js extension."
  (setf args (copy-list args))
  (remf args :destination-file)
  (unless destination-file
    (setf destination-file (merge-pathnames (make-pathname :type "js")
                                            source-file)))
  (with-open-file (output destination-file :if-exists :supersede :direction :output)
    (write-string (apply #'compile-parenscript-file-to-string source-file args) output)))
