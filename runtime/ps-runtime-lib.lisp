(in-package #:parenscript)

;;; Script of library functions you can include with your own code to
;;; provide standard Lisp functionality.

(defparameter *ps-lisp-library*
  '(progn
    (defun mapcar (fun &rest arrs)
      (let ((result-array (make-array)))
        (if (= 1 (length arrs))
            (dolist (element (aref arrs 0))
              ((@ result-array push) (fun element)))
            (dotimes (i (length (aref arrs 0)))
              (let ((args-array (mapcar (lambda (a) (aref a i)) arrs)))
                ((@ result-array push) ((@ fun apply) fun args-array)))))
        result-array))

    (defun map-into (fn arr)
      "Call FN on each element in ARR, replace element with the return value."
      (let ((idx 0))
        (dolist (el arr)
          (setf (aref arr idx) (fn el))
          (setf idx (1+ idx))))
      arr)

    (defun map (fn arr)
      "Call FN on each element in ARR and return the returned values in a new array."
      ;; In newer versions of ECMAScript, this may call Array.map, too
      (let ((idx 0)
            (result (array)))
        (dolist (el arr)
          (setf (aref result idx) (fn el))
          (setf idx (1+ idx)))
        result))

    (defun member (item arr)
      "Check if ITEM is a member of ARR."
      (dolist (el arr)
        (if (= el item)
            (return-from member true)))
      false)

    (defun set-difference (arr arr-to-sub)
      "Return a new array with only those elements in ARR that are not in ARR-TO-SUB."
      (let ((idx 0)
            (result (array)))
        (dolist (el arr)
          (unless (member el arr-to-sub)
            (setf (aref result idx) el)
            (setf idx (1+ idx))))
        result))

    (defun reduce (func list &optional init) ;; the use of init here is actually a bit broken wrt null
      (let* ((acc))
        (do* ((i (if init -1 0) (1+ i))
              (acc (if init init (elt list 0)) (func acc (elt list i))))
             ((>= i (1- (length list)))))
        acc))

    (defun nconc (arr &rest arrs)
    (when (and arr (> (length arr) 0))
      (loop :for other :in arrs :when (and other (> (length other) 0)) :do
        ((@ arr :splice :apply) arr
         (append (list (length arr) (length other)) other))))
    arr)))
