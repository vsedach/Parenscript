(in-package :parenscript)

(define-condition simple-style-warning (simple-condition style-warning)
  ())

(defun warn-deprecated (old-name &optional new-name)
  (warn 'simple-style-warning
        :format-control "~:@(~a~) is deprecated~:[.~;, use ~:@(~a~) instead~]"
        :format-arguments (list old-name new-name new-name)))

(defmacro without-packages (&body body)
  `(let ((ps:*enable-package-system* nil))
    ,@body))

(defmacro defun-js (old-name new-name args &body body)
  `(defun ,old-name ,args
    ,(when (and (stringp (car body)) (< 1 (length body))) ;; docstring
           (car body))
    (warn-deprecated ',old-name ',new-name)
    (without-packages ,@body)))

;;; DEPRECATED INTERFACE ;;;

(defun-js js-equal script-equal (a b)
  (script-equal a b))

(defun-js js-compile compile-script (form)
  (compile-script form :output-spec :javascript))

(defun-js js-compile-list compile-script (form)
  (compile-script form :output-spec :javascript))

(defmacro defjsmacro (&rest args)
  (warn-deprecated 'defjsmacro 'defpsmacro)
  `(defscriptmacro ,@args))

(defmacro js-file (&rest body)
  (warn-deprecated 'js-file)
  `(html
    (:princ
     (js ,@body))))

(defmacro js-script (&rest body)
  (warn-deprecated 'js-script)
  `((:script :type "text/javascript")
    (:princ (format nil "~%// <![CDATA[~%"))
    (:princ (js ,@body))
    (:princ (format nil "~%// ]]>~%"))))

(defmacro js-inline (&rest body)
  (warn-deprecated 'js-inline)
  `(js-inline* '(progn ,@body)))

(defmacro js-inline* (&rest body)
  (warn-deprecated 'js-inline*)
  `(concatenate 'string "javascript:"
    (string-join (js-to-statement-strings (compile-script-form (list 'progn ,@body)) 0) " ")))

(defmacro with-unique-js-names (&rest args)
  (warn-deprecated 'with-unique-js-names 'with-unique-ps-names)
  `(with-unique-ps-names ,@args))

(defmacro gen-js-name (&rest args)
  (warn-deprecated 'gen-js-name 'gen-ps-name)
  `(gen-ps-name ,@args))

(defmacro gen-js-name-string (&rest args)
  (warn-deprecated 'gen-js-name-string 'gen-script-name-string)
  `(gen-script-name-string ,@args))

;;; Functions for setting up and tearing down a persistent compilation environment
(defun setup-persistent-compilation-environment ()
  (setf *compilation-environment* (make-basic-compilation-environment)))

(defun clear-persistent-compilation-environment ()
  (setf *compilation-environment* nil))
