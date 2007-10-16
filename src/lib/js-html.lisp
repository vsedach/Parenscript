;;; Macros for generating HTML from ParenScript code.

(in-package :parenscript)

(defun optimize-string-list (list)
  (let (res
	cur)
    (dolist (node list)
      (when (numberp node)
	(setf node (format nil "~A" node)))
      (cond ((null cur) (setf cur node))
	    ((and (stringp cur)
		  (stringp node))
	     (setf cur (concatenate 'string cur node)))
	    (t (push cur res)
	       (setf cur node))))
    (push cur res)
    (nreverse res)))

(defun process-html-forms (forms)
  (let (res)
    (labels ((handle-form (form)
               (cond ((keywordp form)
                      (push (format nil "<~A/>"
                                    (string-downcase (symbol-name form))) res))

                     ((atom form)
                      (push form res))

                     ((and (consp form)
                           (keywordp (first form)))
                      (let ((node-name (string-downcase (symbol-name (first form)))))
                        (push (format nil "<~A>" node-name) res)
                        (map nil #'handle-form (cdr form))
                        (push (format nil "</~A>" node-name) res)))

                     ((and (consp form)
                           (consp (first form))
                           (keywordp (caar form)))
                      (let ((node-name (string-downcase (symbol-name (caar form)))))
                        (push (format nil "<~A" node-name) res)

                        (loop with attrs = (cdar form)
                              while attrs
                              for attr-name = (pop attrs)
                              for attr-test = (when (not (keywordp attr-name))
                                                (let ((test attr-name))
                                                  (setf attr-name (pop attrs))
                                                  test))
                              for attr-val = (pop attrs)
                              do
                              (if attr-test
                                  (push `(if ,attr-test
                                          (+ ,(format nil " ~A=\"" (string-downcase (symbol-name attr-name)))
                                           ,attr-val
                                           "\"")
                                          "")
                                        res)
                                  (progn 
                                    (push (format nil " ~A=\"" (string-downcase (symbol-name attr-name)))
                                          res)
                                    (push attr-val res)
                                    (push "\"" res))))
                        (push ">" res)
                        (map nil #'handle-form (cdr form))
                        (push (format nil "</~A>" node-name) res)))

                     ((consp form)
                      (push form res)))))
      (map nil #'handle-form forms))
    (cons '+ (optimize-string-list (nreverse res)))))

(define-ps-special-form ps-html (expecting &rest forms)
  (declare (ignore expecting))
  (compile-parenscript-form (process-html-forms forms)))

(defun process-css-forms (proplist)
  (optimize-string-list (butlast
                         (loop for propval on proplist by #'cddr appending
                              (list (string-downcase (symbol-name (first propval)))
                                    ":"
                                    (second propval)
                                    ";")))))


(define-ps-special-form css-inline (expecting &rest forms)
  (declare (ignore expecting))
  (compile-parenscript-form (cons '+ (process-css-forms forms))))
