(in-package "PARENSCRIPT")

(let ((cache (make-hash-table :test 'equal)))
  (defun encode-js-identifier (identifier)
    "Given a string, produces to a valid JavaScript identifier by
following transformation heuristics case conversion. For example,
paren-script becomes parenScript, *some-global* becomes SOMEGLOBAL."
    (or (gethash identifier cache)
        (setf (gethash identifier cache)
              (let ((no-case-conversion nil)
                    (lowercase t)
                    (all-uppercase nil))
                (when (and (not (string= identifier "[]")) ;; HACK
                           (find-if (lambda (x) (find x '(#\. #\[ #\]))) identifier))
                  (warn "Symbol ~A contains one of '.[]' - this compound naming convention is no longer supported by Parenscript!"
                        identifier))
                (acond ((nth-value 1 (cl-ppcre:scan-to-strings "[\\*|\\+](.+)[\\*|\\+](.*)"
                                                               identifier :sharedp t))
                        (setf all-uppercase t
                              identifier (concatenate 'string (aref it 0) (aref it 1))))
                       ((and (> (length identifier) 1)
                             (or (eql (char identifier 0) #\+)
                                 (eql (char identifier 0) #\*)))
                        (setf lowercase nil
                              identifier (subseq identifier 1)))
                       ((and (> (length identifier) 1)
                             (char= #\: (char identifier 0)))
                        (setf no-case-conversion t
                              identifier (subseq identifier 1))))
                (with-output-to-string (acc)
                  (loop for c across identifier
                     do (acond ((eql c #\-)
                                (setf lowercase (not lowercase)))
                               ((position c "!?#@%+*/=:<>")
                                (write-sequence (aref #("bang" "what" "hash" "at" "percent"
                                                        "plus" "star" "slash" "equals" "colon"
                                                        "lessthan" "greaterthan")
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
