(in-package :parenscript)

(defun list-join (list elt)
  (let (res)
    (dolist (i list)
      (push i res)
      (push elt res))
    (pop res)
    (nreverse res)))

(defun list-to-string (list)
  (with-output-to-string (str)
    (dolist (el list)
      (write-string el str))))

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

(defun string-split (string separators &key (keep-separators nil) (remove-empty-subseqs nil))
  (do ((len (length string))
       (i 0 (1+ i))
       (last 0)
       res)
      ((= i len)
       (let ((split (if (> i last)
		     (cons (subseq string last i) res)
		     res)))
         (nreverse (if remove-empty-subseqs
                       (delete "" split :test #'string-equal)
                       split))))
    (when (member (char string i) separators)
      (push (subseq string last i) res)
      (when keep-separators (push (string (char string i)) res))
      (setf last (1+ i)))))

(defparameter *special-chars*
  '((#\! . "Bang")
    (#\? . "What")
    (#\# . "Hash")
    (#\@ . "At")
    (#\% . "Percent")
    (#\+ . "Plus")
    (#\* . "Star")
    (#\/ . "Slash")))


;;; Parenscript-style symbol -> Javascript-style symbol

(defun string-chars (string)
  (coerce string 'list))

(defun constant-string-p (string)
  (let ((len (length string))
        (constant-chars '(#\+ #\*)))
    (and (> len 2)
         (member (char string 0) constant-chars)
         (member (char string (1- len)) constant-chars))))

(defun first-uppercase-p (string)
  (and (> (length string) 1)
       (member (char string 0) '(#\+ #\*))))

(defun untouchable-string-p (string)
  (and (> (length string) 1)
       (char= #\: (char string 0))))

(defun symbol-to-js (symbol)
  "Changes a Parenscript-style symbol or string and converts it to a Javascript-style string.
For example, paren-script becomes parenScript, *some-global* becomes SOMEGLOBAL."
  (when (symbolp symbol)
    (setf symbol (symbol-name symbol)))
  (let ((symbols (string-split symbol '(#\. #\[ #\]) :keep-separators t :remove-empty-subseqs t)))
    (cond ((null symbols) "")
	  ((= (length symbols) 1)
	   (let (res
                 (do-not-touch nil)
		 (lowercase t)
		 (all-uppercase nil))
	     (cond ((constant-string-p symbol)
		    (setf all-uppercase t
			  symbol (subseq symbol 1 (1- (length symbol)))))
		   ((first-uppercase-p symbol)
		    (setf lowercase nil
			  symbol (subseq symbol 1)))
                   ((untouchable-string-p symbol)
                    (setf do-not-touch t
                          symbol (subseq symbol 1))))
	     (flet ((reschar (c)
		      (push (cond
                              (do-not-touch c)
                              ((and lowercase (not all-uppercase))
                               (char-downcase c))
                              (t (char-upcase c)))
                            res)
		      (setf lowercase t)))
	       (dotimes (i (length symbol))
		 (let ((c (char symbol i)))
		   (cond
		     ((eql c #\-)
		      (setf lowercase (not lowercase)))
		     ((assoc c *special-chars*)
		      (dolist (i (coerce (cdr (assoc c *special-chars*)) 'list))
			(reschar i)))
		     (t (reschar c))))))
	     (coerce (nreverse res) 'string)))
	  (t (string-join (mapcar #'symbol-to-js symbols) "")))))
