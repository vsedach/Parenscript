(in-package :parenscript)

;;; Handy utilities for doing common tasks found in many web browser
;;; JavaScript implementations

(defpsmacro do-set-timeout ((timeout) &body body)
  `(set-timeout (lambda () ,@body) ,timeout))

;;; Arithmetic

(defmacro def-js-maths (&rest mathdefs)
  `(progn ,@(mapcar (lambda (def) (cons 'defpsmacro def)) mathdefs)))

(def-js-maths
    (max (&rest nums) `(*math.max ,@nums))
    (min (&rest nums) `(*math.min ,@nums))
    (floor (n &optional divisor) `(*math.floor ,(if divisor `(/ ,n ,divisor) n)))
    (ceiling (n &optional divisor) `(*math.ceil ,(if divisor `(/ ,n ,divisor) n)))
    (round (n &optional divisor) `(*math.round ,(if divisor `(/ ,n ,divisor) n)))
    (sin (n) `(*math.sin ,n))
    (cos (n) `(*math.cos ,n))
    (tan (n) `(*math.tan ,n))
    (asin (n) `(*math.asin ,n))
    (acos (n) `(*math.acos ,n))
    (atan (y &optional x) (if x `(*math.atan2 ,y ,x) `(*math.atan ,y)))
    (sinh (n) `((lambda (x) (return (/ (- (exp x) (exp (- x))) 2))) ,n))
    (cosh (n) `((lambda (x) (return (/ (+ (exp x) (exp (- x))) 2))) ,n))
    (tanh (n) `((lambda (x) (return (/ (- (exp x) (exp (- x))) (+ (exp x) (exp (- x)))))) ,n))
    (asinh (n) `((lambda (x) (return (log (+ x (sqrt (1+ (* x x))))))) ,n))
    (acosh (n) `((lambda (x) (return (* 2 (log (+ (sqrt (/ (1+ x) 2)) (sqrt (/ (1- x) 2))))))) ,n))
    (atanh (n) `((lambda (x) (return (/ (- (log (+ 1 x)) (log (- 1 x))) 2))) ,n))
    (1+ (n) `(+ ,n 1))
    (1- (n) `(- ,n 1))
    (abs (n) `(*math.abs ,n))
    (evenp (n) `(not (oddp ,n)))
    (oddp (n) `(% ,n 2))
    (exp (n) `(*math.exp ,n))
    (expt (base power) `(*math.pow ,base ,power))
    (log (n &optional base)
      (or (and (null base) `(*math.log ,n))
          (and (numberp base) (= base 10) `(* (log ,n) *math.*log10e*))
          `(/ (log ,n) (log ,base))))
    (sqrt (n) `(*math.sqrt ,n))
    (random (&optional upto) (if upto
                                 `(floor (* ,upto (*math.random)))
                                 '(*math.random))))

(define-ps-symbol-macro pi '*math.*pi*)

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

(defpsmacro concatenate (result-type &rest sequences)
  (assert (equal result-type ''string) () "Right now Parenscript 'concatenate' only support strings.")
  (cons '+ sequences))
