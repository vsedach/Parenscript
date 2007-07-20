(in-package :parenscript)

(defun make-css-rule (selectors properties)
  (list (mapcar #'val-to-string
		(if (atom selectors)
		    (list selectors)
		    selectors))
	properties))

(defun css-rule-selectors (css-rule)
  (first css-rule))

(defun css-rule-properties (css-rule)
  (second css-rule))

(defmacro css-rule (selectors &rest properties)
  `(make-css-rule ',selectors ',properties))

(defun propval-to-string (propval)
  (format nil "~A:~A" (val-to-string (first propval))
	  (val-to-string (second propval))))

(defun css-rule-to-string (css-rule)
  (format nil "~A {~%~{~A;~%~}}~%~%"
	  (string-join (css-rule-selectors css-rule) ",")
	  (loop for propval on (css-rule-properties css-rule) by #'cddr
		    collect (concatenate 'string "   " (propval-to-string propval)))))

(defun css-to-string (rules)
  (string-join (mapcar #'css-rule-to-string rules) "; "))

(defmacro css (&rest rules)
  `((:style :type "text/css")
    (:princ #\Newline "<!--" #\Newline)
    (:princ ,@(mapcar #'(lambda (rule) `(css-rule-to-string (css-rule ,@rule))) rules))
    (:princ "-->" #\Newline)))

(defun css-inline-func (proplist)
  (string-join (loop for propval on proplist by #'cddr
		     collect (propval-to-string propval))
	       ";"))

(defmacro css-inline (&rest propvals)
  `(parenscript::css-inline-func ,propvals))

(defmacro css-file (&rest rules)
  `(html
    (:princ
     ,@(mapcar #'(lambda (rule) `(css-rule-to-string (css-rule ,@rule))) rules))))

;;; examples

;;; generate a CSS file
#+nil
(html-stream *standard-output*
      (css-file (* :border "1px solid black")
	    (div.bl0rg :font-family "serif")
	    (("a:active" "a:hoover") :color "black" :size "200%")))


;;; generate an inline CSS spec in a HTML head element
#+nil
(html-stream *standard-output*
  (html
   (:html
    (:head
     (css (* :border "1px solid black")
	  (div.bl0rg :font-family "serif")
	  (("a:active" "a:hoover") :color "black" :size "200%"))))))

;;; generate a style attribute for a DIV element
#+nil
(html-stream *standard-output*
      (html (:html (:body ((:div :style (css-inline :border "1px solid black"))
			   "foobar")))))

