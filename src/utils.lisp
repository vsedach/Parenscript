(in-package "PARENSCRIPT")

(defun string-join (strings separator)
  (format nil "~{~}" (format nil "~~a~~^~a" separator) strings))

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
    (#\= . "Equals")
    (#\: . "Colon")))

;;; Parenscript-style symbol -> Javascript-style symbol

(defun special-symbol-delimiter? (char)
  (or (eql char #\+) (eql char #\*)))

(defun special-symbol-name? (string)
  (nth-value 1 (cl-ppcre:scan-to-strings "[\\*|\\+](.*)[\\*|\\+](.*)" string)))

(defun first-uppercase-p (string)
  (and (> (length string) 1)
       (special-symbol-delimiter? (char string 0))))

(defun untouchable-string-p (string)
  (and (> (length string) 1)
       (char= #\: (char string 0))))

(defun symbol-name-to-js-string (symbol)
  "Given a Lisp symbol or string, produces to a valid JavaScript
identifier by following transformation heuristics case conversion. For
example, paren-script becomes parenScript, *some-global* becomes
SOMEGLOBAL."
  (let ((sym-name (string symbol))
        res
        (do-not-touch nil)
        (lowercase t)
        (all-uppercase nil))
    (when (and (not (eq symbol '[]))
               (find-if (lambda (x) (member x '(#\. #\[ #\]))) sym-name))
      (warn "Symbol ~A contains one of '.[]' - this compound naming convention is no longer supported by Parenscript!"
            symbol))
    (cond ((special-symbol-name? sym-name)
           (setf all-uppercase t
                 sym-name (let ((parts (special-symbol-name? sym-name)))
                            (concatenate 'string (aref parts 0) (aref parts 1)))))
          ((first-uppercase-p sym-name)
           (setf lowercase nil
                 sym-name (subseq sym-name 1)))
          ((untouchable-string-p sym-name)
           (setf do-not-touch t
                 sym-name (subseq sym-name 1))))
    (flet ((reschar (c)
             (push (cond (do-not-touch c)
                         ((and lowercase (not all-uppercase)) (char-downcase c))
                         (t (char-upcase c)))
                   res)
             (setf lowercase t)))
      (dotimes (i (length sym-name))
        (let ((c (char sym-name i)))
          (cond ((eql c #\-)
                 (setf lowercase (not lowercase)))
                ((assoc c *special-chars*)
                 (dolist (i (coerce (cdr (assoc c *special-chars*)) 'list))
                   (reschar i)))
                (t (reschar c))))))
    (coerce (nreverse res) 'string)))

(defun ordered-set-difference (list1 list2 &key (test #'eql)) ; because the CL set-difference may not preserve order
  (reduce (lambda (list el) (remove el list :test test))
          (cons list1 list2)))

(defmacro once-only ((&rest names) &body body) ;; the version from PCL
  (let ((gensyms (loop for nil in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

(defun flatten (x)
  (if (atom x)
      (list x)
      (mapcan #'flatten x)))
