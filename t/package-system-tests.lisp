(in-package :parenscript-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite package-system-tests))

(in-suite package-system-tests)

(test-ps-js operator-packages1
  (#:new)
  "new()")

(defpackage "MY-LIBRARY"
  (:use #:parenscript))
(setf (ps-package-prefix :my-library) "my_library_")

(test-ps-js lib-function1
  (defun my-library::library-function (x y)
    (return (+ x y)))
  "function my_library_libraryFunction(x, y) {
        return x + y;
     }")

(defpackage "OBFUSCATE-ME")
(obfuscate-package :obfuscate-me)

(test-ps-js obfuscation1
  (defun obfuscate-me::library-function2 (a b obfuscate-me::foo)
    (+ a (my-library::library-function b obfuscate-me::foo)))
  "function g2(a, b, g3) {
    a + my_library_libraryFunction(b, g3);
}")

(defpackage "OBFUSCATE-AND-PREFIX")
(obfuscate-package "OBFUSCATE-AND-PREFIX")
(setf (ps-package-prefix "OBFUSCATE-AND-PREFIX") "__FOO___")

(test-ps-js obfuscate-and-prefix
  (defun obfuscate-and-prefix::some-function (a obfuscate-and-prefix::b my-library::d)
    (* a
       (obfuscate-me::library-function2 obfuscate-and-prefix::b a)
       (my-library::library-function my-library::d obfuscate-and-prefix::b)))
  "function __FOO___g2(a, __FOO___g3, my_library_d) {
    a * g2(__FOO___g3, a) * my_library_libraryFunction(my_library_d, __FOO___g3);
}")

(defpackage "PSTSTPKG"
  (:use #:parenscript))

(test namespace1 ()
  (setf (ps-package-prefix "PSTSTPKG") "prefix_")
  (is (string= "prefix_foo;" (normalize-js-code (ps* 'pststpkg::foo)))))

(common-lisp:in-package "PSTSTPKG")

(ps-test::test-ps-js namespace-and-special-forms
  (let* ((foo (create :bar 1 not-a-keyword something)))
    (return (and (not foo) (+ (slot-value foo bar) some-other-var))))
  "        var prefix_foo =
            { bar : 1, 
              prefix_notAKeyword : prefix_something };
        return !prefix_foo && prefix_foo[prefix_bar] + prefix_someOtherVar;")
