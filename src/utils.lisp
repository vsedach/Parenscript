(in-package "PARENSCRIPT")

(let ((cache (make-hash-table :test 'eq)))
  (defun symbol-name-to-js-string (symbol)
    "Given a Lisp symbol or string, produces to a valid JavaScript
identifier by following transformation heuristics case conversion. For
example, paren-script becomes parenScript, *some-global* becomes
SOMEGLOBAL."
    (or (gethash symbol cache)
        (setf (gethash symbol cache)
              (let ((sym-name (symbol-name symbol))
                    (no-case-conversion nil)
                    (lowercase t)
                    (all-uppercase nil))
                (when (and (not (eq symbol '[])) ;; HACK
                           (find-if (lambda (x) (find x '(#\. #\[ #\]))) sym-name))
                  (warn "Symbol ~A contains one of '.[]' - this compound naming convention is no longer supported by Parenscript!"
                        symbol))
                (acond ((nth-value 1 (cl-ppcre:scan-to-strings "[\\*|\\+](.*)[\\*|\\+](.*)"
                                                               sym-name :sharedp t))
                        (setf all-uppercase t
                              sym-name (concatenate 'string (aref it 0) (aref it 1))))
                       ((and (> (length sym-name) 1)
                             (or (eql (char sym-name 0) #\+)
                                 (eql (char sym-name 0) #\*)))
                        (setf lowercase nil
                              sym-name (subseq sym-name 1)))
                       ((and (> (length sym-name) 1)
                             (char= #\: (char sym-name 0)))
                        (setf no-case-conversion t
                              sym-name (subseq sym-name 1))))
                (with-output-to-string (acc)
                  (loop for c across sym-name
                     do (acond ((eql c #\-)
                                (setf lowercase (not lowercase)))
                               ((position c "!?#@%+*/=:")
                                (write-sequence (aref #("bang" "what" "hash" "at" "percent"
                                                        "plus" "star" "slash" "equals" "colon")
                                                      it)
                                                acc))
                               (t (write-char (cond (no-case-conversion c)
                                                    ((and lowercase (not all-uppercase)) (char-downcase c))
                                                    (t (char-upcase c)))
                                              acc)
                                  (setf lowercase t))))))))))
  
(defun ordered-set-difference (list1 list2 &key (test #'eql)) ; because the CL set-difference may not preserve order
  (reduce (lambda (list el) (remove el list :test test))
          (cons list1 list2)))

(defmacro once-only ((&rest names) &body body) ;; the version from PCL
  (let ((gensyms (loop for nil in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

(defun flatten (x &optional acc)
  (cond ((null x) acc)
        ((atom x) (cons x acc))
        (t (flatten (car x) (flatten (cdr x) acc)))))
