(in-package :parenscript)

;;; Script of library functions you can include with your own code to
;;; provide standard Lisp functionality.

(defparameter *ps-lisp-library*
  '(progn
    (defun mapcar (fun &rest as)
      (let ((result-array (make-array)))
        (if (= 1 (length as))
            (dolist (element (aref as 0))
              (result-array.push (fun element)))
            (dotimes (i (length (aref as 0)))
              (let ((args-array (mapcar (lambda (a) (return (aref a i))) as)))
                (result-array.push (fun.apply fun args-array)))))
        (return result-array)))

    (defun map-into (fn arr)
      "Call FN on each element in ARR, replace element with the return value."
      (let ((idx 0))
        (dolist (el arr)
          (setf (aref arr idx) (fn el))
          (setf idx (1+ idx))))
      (return arr))

    (defun map (fn arr)
      "Call FN on each element in ARR and return the returned values in a new array."
      ;; In newer versions of ECMAScript, this may call Array.map, too
      (let ((idx 0)
            (result (array)))
        (dolist (el arr)
          (setf (aref result idx) (fn el))
          (setf idx (1+ idx)))
        (return result)))

    (defun map-until (fn arr)
      "Call FN on each element in ARR until it returns something. If so return that value."
      (let ((result))
        (dolist (el arr)
          (setf result (fn el))
          (unless (= result undefined)
            (return result)))))

    (defun member (item arr)
      "Check if ITEM is a member of ARR."
      (dolist (el arr)
        (if (= el item)
            (return true)))
      (return false))

    (defun append (arr1 arr2)
      "Return a new array with the contents of ARR1 and ARR2. If ARR2 is not an array
then append it as a member."
      (let ((result (array))
            (idx 0))
        (dolist (el arr1)
          (setf (aref result idx) el)
          (setf idx (1+ idx)))
        (unless (= arr2 undefined)
          (if (instanceof arr2 *array)
              (dolist (el arr2)
                (setf (aref result idx) el)
                (setf idx (1+ idx)))
              (setf (aref result idx) arr2))))
      (return result))

    (defun set-difference (arr arr-to-sub)
      "Return a new array with only those elements in ARR that are not in ARR-TO-SUB."
      (let ((idx 0)
            (result (array)))
        (dolist (el arr)
          (unless (member el arr-to-sub)
            (setf (aref result idx) el)
            (setf idx (1+ idx))))
        (return result)))

    (defun reduce (func list &optional init) ;; the use of init here is actually a bit broken wrt null
      (let* ((acc))
        (do* ((i (if init -1 0) (1+ i))
              (acc (if init init (elt list 0)) (func acc (elt list i))))
             ((>= i (1- (length list)))))
        (return acc)))))
