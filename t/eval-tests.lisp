;; Copying and distribution of this file, with or without
;; modification, are permitted in any medium without royalty. This
;; file is offered as-is, without any warranty.

(in-package #:ps-eval-tests)
(named-readtables:in-readtable :parenscript)

(in-suite ps-test::eval-tests)

(test-js-eval number
  3
  3)

(test-js-eval float
  (< 123.12 123.123 123.124)
  t)

(test-js-eval string
  "foobar"
  "foobar")

(test-js-eval statements-and-expressions-2
  ((lambda () (if 1 2 3)))
  2)

(test-js-eval statements-and-expressions-1
  ((lambda () (+ 2 (if 1 2 3))))
  4)

(test-js-eval empty-array
  (array)
  (js-array (make-array 0 :adjustable t)))

(test-js-eval funargs-let1
  ((lambda (x)
     (let ((x 10))
       (incf x))
     (incf x)) 0)
  1)

(test-js-eval times-rem
  (* 12 (rem 10 3))
  12)

(test-js-eval divide-rem
  (rem 10 (/ 12 4))
  1)

(test-js-eval rem-divide
  (/ 9 (rem 11 4))
  3)

(test-js-eval rem-multiply
  (* 5 (rem 4 3))
  5)

(test-js-eval case-return-break-null
  ((lambda (x) (case x (1) (2 3))) 1)
  :null)

(test-js-eval defun-return1
  (progn (defun bar (x) (1+ x))
         (bar 1))
  2)

(test-js-eval defun-return2
  (progn (defun bar (fn)
           (funcall fn))

         (defun foo ()
           (bar (lambda () (return-from foo 123))))

         (foo))
  123)

(test-js-eval defvar
  (progn (defvar foo 1)
         foo)
  1)

(test-js-eval block-dynamic-lambda
  ((lambda () (block nil (return 4) (+ 1 2))))
  4)

(test-js-eval block-dynamic-lambda1
  ((lambda () (block nil (return 4) (+ 1 2)) 5))
  5)

(test-js-eval loop-sum
  (+ 1 (loop for i from 0 to 10 sum i))
  56)

(test-js-eval loop-funcall-sum
  ((lambda (x) (1+ x)) (loop for i from 0 to 10 sum i))
  56)

(test-js-eval loop-funcall-if-sum
  (progn (defun one-plus (x)
           (1+ x))

         (one-plus (if false 1 (loop for i from 0 to 10 sum i))))
  56)

;;; broken

(test-js-eval funcall-loop-doing
  ((lambda (x) x)
   (loop for i from 0 to 10 do (1+ i)))
  :null)

(test-js-eval block-dynamic-lambda2
  ((lambda () (1+ (block nil (return 4) (+ 1 2)))))
  5)

(test-js-eval block-dynamic-return
  (block nil (return 4) (+ 1 2))
  4)

(test-js-eval block-lambda-dynamic-return
  (block nil ((lambda () (return 4))) (+ 1 2))
  4)

(test-js-eval block-dynamic-setf
  (progn (defvar foo (block nil (return 4) (+ 1 2)))
         foo)
  4)

(test-js-eval block-lambda-dynamic-setf
  (progn (defvar foo (block nil ((lambda () (return 4))) (+ 1 2)))
         foo)
  4)

(test-js-eval block-lambda-dynamic-lambda
  ((lambda () (block nil ((lambda () (return 4))) (+ 1 2))))
  4)
