;; Description:
;;   Javascript html generator

(in-package :js)

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
		      (loop for (attr-name attr-val) on (cdar form) by #'cddr
			    do (unless (keywordp attr-name)
				 (error "~A is not a node attribute" attr-name))
			    (push (format nil " ~A=\"" (string-downcase (symbol-name attr-name)))
				  res)
			    (push attr-val res)
			    (push "\"" res))
		      (push ">" res)
		      (map nil #'handle-form (cdr form))
		      (push (format nil "</~A>" node-name) res)))

		   ((consp form)
		    (push form res)))))
      (map nil #'handle-form forms))
    (cons '+ (optimize-string-list (nreverse res)))))

(define-js-compiler-macro html (&rest forms)
  (js-compile (process-html-forms forms)))
