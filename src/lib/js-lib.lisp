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
        (return result-array)))))
