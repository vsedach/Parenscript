(in-package :js)

(defun compile-parenscript-file-to-string (source-file &key (log-stream nil) (comment nil))
  "Compile a parenscript file to a javascript string. (in-package ...) forms
behave as expected and all other forms are evaluated. If the result of the
evaluation is not nil tehn it's compiled with js:js* and written to the output."
  (with-output-to-string (output)
    (with-open-file (input source-file :direction :input)
      (flet ((read-form ()
               (read input nil))
             (log-message (&rest args)
               (when log-stream
                 (apply #'format log-stream args))))
        (let ((saved-package *package*))
          (unwind-protect
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
                           (setf form (eval form))
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
                           (setf *package* (find-package (cadr form))))))
            (setf *package* saved-package)))))))

(defun compile-parenscript-file (source-file &key destination-file (log-stream nil) (comment nil))
  "Compile a parenscript file to a javascript file with
compile-parenscript-file-to-string. When DESTINATION-FILE is omitted,
then it will be named the same as the SOURCE-FILE but with js extension."
  (unless destination-file
    (setf destination-file (merge-pathnames (make-pathname :type "js")
                                            source-file)))
  (with-open-file (output destination-file :if-exists :supersede :direction :output)
    (write-string (compile-parenscript-file-to-string source-file :log-stream log-stream :comment comment) output)))
