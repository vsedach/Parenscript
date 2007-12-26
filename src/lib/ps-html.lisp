(in-package :parenscript)

(defun process-html-forms (forms) ;; this needs a rewrite
  (let ((res ()))
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
      (map nil #'handle-form forms)
      (concat-constant-strings (reverse res)))))

(defpsmacro ps-html (&rest html-forms)
  (cons '+ (process-html-forms html-forms)))

(defmacro ps-html (&rest html-forms)
  `(format nil "~@{~A~}" ,@(process-html-forms html-forms)))
