(in-package #:parenscript)

(let ((cache (make-hash-table :test 'equal)))
  (defun encode-js-identifier (identifier)
    "Given a string, produces to a valid JavaScript identifier by
following transformation heuristics case conversion. For example,
paren-script becomes parenScript, *some-global* becomes SOMEGLOBAL."
    (or (gethash identifier cache)
        (setf (gethash identifier cache)
              (cond ((some (lambda (c) (find c "-*+!?#@%/=:<>^")) identifier)
                     (let ((lowercase t)
                           (all-uppercase nil))
                       (when (and (not (string= identifier "[]")) ;; HACK
                                  (find-if (lambda (x) (find x '(#\. #\[ #\]))) identifier))
                         (warn "Symbol ~A contains one of '.[]' - this compound naming convention is no longer supported by Parenscript!"
                               identifier))
                       (acond ((nth-value 1 (cl-ppcre:scan-to-strings "[\\*|\\+](.+)[\\*|\\+](.*)" identifier :sharedp t))
                               (setf all-uppercase t
                                     identifier (concatenate 'string (aref it 0) (aref it 1))))
                              ((and (> (length identifier) 1)
                                    (or (eql (char identifier 0) #\+)
                                        (eql (char identifier 0) #\*)))
                               (setf lowercase nil
                                     identifier (subseq identifier 1))))
                       (with-output-to-string (acc)
                         (loop for c across identifier
                            do (acond ((eql c #\-)
                                       (setf lowercase (not lowercase)))
                                      ((position c "!?#@%+*/=:<>^")
                                       (write-sequence (aref #("bang" "what" "hash" "at" "percent"
                                                               "plus" "star" "slash" "equals" "colon"
                                                               "lessthan" "greaterthan" "caret")
                                                             it)
                                                       acc))
                                      (t (write-char (cond ((and lowercase (not all-uppercase)) (char-downcase c))
                                                           (t (char-upcase c)))
                                                     acc)
                                         (setf lowercase t)))))))
                    ((every #'upper-case-p (remove-if-not #'alpha-char-p identifier)) (string-downcase identifier))
                    ((every #'lower-case-p (remove-if-not #'alpha-char-p identifier)) (string-upcase identifier))
                    (t identifier))))))

(defun ordered-set-difference (list1 list2 &key (test #'eql))
  "CL set-difference may not preserve order."
  (reduce (lambda (list el) (remove el list :test test))
          (cons list1 list2)))

(defun flatten (x &optional acc)
  (cond ((null x) acc)
        ((atom x) (cons x acc))
        (t (flatten (car x) (flatten (cdr x) acc)))))

(defun tree-search (A tree)
  (or (equal A tree)
      (when (consp tree)
        (loop for x on tree thereis ;; fucking dotted lists
              (or (tree-search A (car x))
                  (unless (listp (cdr x))
                    (equal A (cdr x))))))))

(labels ((compare (a b op equality)
           (if (not (stringp a)) (setf a (format nil "~A" a)))
           (if (not (stringp b)) (setf b (format nil "~A" b)))
           (loop with i := 0 and j := 0 and m and n 
                 while (or i j)
                 do (multiple-value-setq (m i)
                      (if (and i (< i (length a)))
                          (parse-integer a :start i :junk-allowed t)))
                    (multiple-value-setq (n j)
                      (if (and j (< j (length b)))
                          (parse-integer b :start j :junk-allowed t)))
                 if (not m) do (setf m -1) else do (incf i)
                 if (not n) do (setf n -1) else do (incf j)
                 do (let ((op-p (funcall op m n))
                          (rev-op-p (funcall op n m)))
                      (cond
                        ((and op-p (not (and rev-op-p equality)))
                         (return t))
                        ((and rev-op-p (not (and op-p equality)))
                         (return nil))
                        ((and (not (or op-p rev-op-p)) equality)
                         (return nil))))
                 finally (return equality))))
  (defun vstring< (a b) (compare a b #'< nil))
  (defun vstring<= (a b) (compare a b #'<= t))
  (defun vstring= (a b) (compare a b #'= t))
  (defun vstring>= (a b) (compare a b #'>= t))
  (defun vstring> (a b) (compare a b #'> nil)))
