(in-package "PARENSCRIPT-TEST")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite package-system-tests))

(in-suite package-system-tests)

(test-ps-js operator-packages1
  (#:new)
  "new();")

(defpackage "PS-TEST.MY-LIBRARY"
  (:use "PARENSCRIPT"))
(setf (ps-package-prefix "PS-TEST.MY-LIBRARY") "my_library_")

(test-ps-js lib-function1
  (defun ps-test.my-library::library-function (x y)
    (return (+ x y)))
  "function my_library_libraryFunction(x, y) {
        return x + y;
     };")

(test-ps-js uniform-symbol-handling1
  (progn (create 'ps-test.my-library::foo 1)
         (create ps-test.my-library::foo 1)
         (slot-value foo 'ps-test.my-library::foo))
  "{ 'my_library_foo' : 1 };
{ my_library_foo : 1 };
foo.my_library_foo;")

(defpackage "PS-TEST.OBFUSCATE-ME")
(obfuscate-package "PS-TEST.OBFUSCATE-ME")

(test-ps-js obfuscation1
  (defun ps-test.obfuscate-me::library-function2 (a b ps-test.obfuscate-me::foo)
    (+ a (ps-test.my-library::library-function b ps-test.obfuscate-me::foo)))
  "function g1(a, b, g2) {
    a + my_library_libraryFunction(b, g2);
};")

(defpackage "PS-TEST.OBFUSCATE-AND-PREFIX")
(obfuscate-package "PS-TEST.OBFUSCATE-AND-PREFIX")
(setf (ps-package-prefix "PS-TEST.OBFUSCATE-AND-PREFIX") "__FOO___")

(test-ps-js obfuscate-and-prefix
  (defun ps-test.obfuscate-and-prefix::some-function (a ps-test.obfuscate-and-prefix::b ps-test.my-library::d)
    (* a
       (ps-test.obfuscate-me::library-function2 ps-test.obfuscate-and-prefix::b a)
       (ps-test.my-library::library-function ps-test.my-library::d ps-test.obfuscate-and-prefix::b)))
  "function __FOO___g1(a, __FOO___g2, my_library_d) {
    a * g1(__FOO___g2, a) * my_library_libraryFunction(my_library_d, __FOO___g2);
};")

(defpackage "PS-TEST.PSTSTPKG"
  (:use "PARENSCRIPT"))

(test namespace1 ()
  (setf (ps-package-prefix "PS-TEST.PSTSTPKG") "prefix_")
  (is (string= "prefix_foo;" (normalize-js-code (ps* 'ps-test.pststpkg::foo)))))

(common-lisp:in-package "PS-TEST.PSTSTPKG")

(ps-test::test-ps-js namespace-and-special-forms
  (let ((foo (create bar 1 not-a-keyword something)))
    (return (and (not foo) (+ (slot-value foo 'bar) some-other-var))))
"var prefix_foo = { prefix_bar : 1, prefix_notAKeyword : prefix_something };
return !prefix_foo && prefix_foo.prefix_bar + prefix_someOtherVar;")
