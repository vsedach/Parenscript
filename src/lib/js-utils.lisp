(in-package :parenscript)

;;; Handy utilities for doing common tasks found in many web browser
;;; JavaScript implementations

(defscriptmacro do-set-timeout ((timeout) &body body)
  `(set-timeout (lambda () ,@body) ,timeout))

;;; Arithmetic

(defmacro def-js-maths (&rest mathdefs)
  `(progn ,@(mapcar (lambda (def) (cons 'defscriptmacro def)) mathdefs)))

(def-js-maths
    (min (&rest nums) `(*math.min ,@nums))
    (max (&rest nums) `(*math.max ,@nums))
    (ceiling (n &optional divisor) `(*math.ceil ,(if divisor `(/ ,n ,divisor) n)))
    (abs (n) `(*math.abs ,n))
    (sin (n) `(*math.sin ,n))
    (cos (n) `(*math.cos ,n))
    (tan (n) `(*math.tan ,n))
    (acos (n) `(*math.acos ,n))
    (asin (n) `(*math.asin ,n))
    (atan (n) `(*math.atan ,n))
    (exp (n) `(*math.exp ,n))
    (floor (n &optional divisor) `(*math.floor ,(if divisor `(/ ,n ,divisor) n)))
    (expt (base power) `(*math.pow ,base ,power))
    (round (n &optional divisor) `(*math.round ,(if divisor `(/ ,n ,divisor) n)))
    (random (&optional upto) (if upto
                                 `(floor (* ,upto (*math.random)))
                                 '(*math.random)))
    (oddp (n) `(% ,n 2))
    (evenp (n) `(not (oddp ,n))))

;;; Exception handling

(defscriptmacro ignore-errors (&body body)
  `(try (progn ,@body) (:catch (e))))
