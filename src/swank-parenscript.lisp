(in-package :parenscript)

(defun parenscript-function-p (symbol)
  (and (or (gethash symbol *ps-macro-toplevel* )
           (gethash symbol *ps-function-toplevel-cache*))
       t))
#++
(pushnew 'parenscript-function-p swank::*external-valid-function-name-p-hooks*)

(defun parenscript-arglist (fname)
  (acond
    ((gethash fname *ps-macro-toplevel-lambda-list*)
     (values it t))
    ((gethash fname *ps-function-toplevel-cache*)
     (values it t))))
#++
(pushnew 'parenscript-arglist swank::*external-arglist-hooks*)

