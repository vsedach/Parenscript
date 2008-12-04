(in-package :parenscript)

(defun string-join (strings separator)
  (format nil "~{~}" (format nil "~~a~~^~a" separator) strings))

(defun val-to-string (val)
  (if (symbolp val)
      (string-downcase (symbol-name val))
      (princ-to-string val)))

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
    (#\/ . "Slash")
    (#\= . "Equals")))

;;; Parenscript-style symbol -> Javascript-style symbol

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

(defun symbol-to-js-string (symbol)
  "Given a Lisp symbol or string, produces to a valid JavaScript
identifier by following transformation heuristics case conversion. For
example, paren-script becomes parenScript, *some-global* becomes
SOMEGLOBAL."
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
          (t (string-join (mapcar #'symbol-to-js-string symbols) "")))))

(defun ordered-set-difference (list1 list2 &key (test #'eql)) ; because the CL set-difference may not preserve order
  (reduce (lambda (list el) (remove el list :test test))
          (cons list1 list2)))
