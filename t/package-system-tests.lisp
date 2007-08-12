(in-package :parenscript-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite package-system-tests))

(in-suite package-system-tests)

(defpackage "PSTSTPKG"
  (:use #:parenscript))

(test namespace1 ()
  (setf (ps-package-prefix "PSTSTPKG") "prefix_")
  (is (string= "prefix_var;" (normalize-js-code (ps pststpkg::var)))))

(common-lisp:in-package "PSTSTPKG")

(ps-test::test-ps-js namespace-and-special-forms
  (let ((foo (create :bar 1 not-a-keyword something)))
    (return (and (not foo) (+ (slot-value foo bar) some-other-var))))
  "        var prefix_foo =
            { bar : 1, 
              prefix_notAKeyword : prefix_something };
        return !prefix_foo && prefix_foo[prefix_bar] + prefix_someOtherVar;")

