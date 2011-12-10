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

(test-js-eval block-dynamic-return
  (block nil ((lambda () (return))) (+ 1 2))
  :null)

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
