(in-package :parenscript)

;;; Handy utilities for doing common tasks found in many web browser
;;; JavaScript implementations

(defpsmacro do-set-timeout ((timeout) &body body)
  `(set-timeout (lambda () ,@body) ,timeout))

;;; Arithmetic

(defmacro def-js-maths (&rest mathdefs)
  `(progn ,@(mapcar (lambda (def) (cons 'defpsmacro def)) mathdefs)))

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

(defpsmacro ignore-errors (&body body)
  `(try (progn ,@body) (:catch (e))))

;;; Data structures

(defpsmacro length (a)
  `(.size ,a))

;;; Misc

(defpsmacro null (x)
  `(= ,x nil))

(defpsmacro @ (obj &rest props)
  "Handy slot-value/aref composition macro."
  (if (null props)
      obj
      `(@ (slot-value
           ,(if (stringp obj) `($ ,obj) obj)
           ,(let ((prop (macroexpand (first props))))
                 (if (symbolp prop)
                     `',prop
                     prop)))
        ,@(cdr props))))