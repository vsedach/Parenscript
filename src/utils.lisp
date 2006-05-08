(in-package :js)

(defun list-join (list elt)
  (let (res)
    (dolist (i list)
      (push i res)
      (push elt res))
    (pop res)
    (nreverse res)))

;;; wie herrlich effizient
(defun list-to-string (list)
  (reduce #'(lambda (str1 &optional (str2 ""))
              (concatenate 'string str1 str2))
          list))

(defun append-to-last (form elt)
  (cond ((stringp form)
	 (concatenate 'string form elt))
	((consp form)
	 (let ((last (last form)))
	   (if (stringp (car last))
	       (rplaca last (concatenate 'string (car last) elt))
	       (append-to-last (car last) elt))
	   form))
	(t (error "unsupported form ~S" form))))

(defun prepend-to-first (form elt)
  (cond ((stringp form)
	 (concatenate 'string elt form))
	((consp form)
	 (let ((first (first form)))
	   (if (stringp first)
	       (rplaca form (concatenate 'string elt first))
	       (prepend-to-first first elt))
	   form))
	(t (error "unsupported form ~S" form))))

(defun string-join (strings elt)
  (list-to-string (list-join strings elt)))

(defun val-to-string (val)
  (cond ((stringp val) val)
	((symbolp val) (string-downcase (symbol-name val)))
	(t (princ-to-string val))))

(defun string-split (string separators)
  (do ((len (length string))
       (i 0 (1+ i))
       (last 0)
       res)
      ((= i len)
       (nreverse (if (> i last)
		     (cons (subseq string last i) res)
		     res)))
    (when (member (char string i) separators)
      (push (subseq string last i) res)
      (setf last (1+ i)))))

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


