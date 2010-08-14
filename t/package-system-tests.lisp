(in-package #:parenscript-test)

(in-suite package-system-tests)

(test-ps-js operator-packages1
  (#:new)
  "new();")

(defpackage #:ps-test.my-library
  (:use #:parenscript))
(setf (ps-package-prefix '#:ps-test.my-library) "my_library_")

(test-ps-js lib-function1
  (defun ps-test.my-library::library-function (x y)
    (return (+ x y)))
  "function my_library_libraryFunction(x, y) {
        return x + y;
     };")

(test-ps-js uniform-symbol-handling1
  (progn (create ps-test.my-library::foo 1)
         (getprop foo 'ps-test.my-library::foo))
  "{ my_library_foo : 1 };
foo.my_library_foo;")

(let ((map (make-hash-table)))
  (defun symbol-obfuscator (symbol)
    (or #1=(gethash symbol map)
        (setf #1# (make-symbol (map 'string (lambda (x)
                                              (code-char (1+ (char-code x))))
                                    (symbol-name symbol)))))))

(defpackage #:ps-test.obfuscate-me)
(obfuscate-package '#:ps-test.obfuscate-me #'symbol-obfuscator)

(test-ps-js obfuscation1
  (defun ps-test.obfuscate-me::libfun2 (a b ps-test.obfuscate-me::foo)
    (+ a (ps-test.my-library::library-function b ps-test.obfuscate-me::foo)))
  "function mjcgvo3(a, b, gpp) {
    return a + my_library_libraryFunction(b, gpp);
};")

(defpackage #:ps-test.obfuscate-and-prefix)
(obfuscate-package '#:ps-test.obfuscate-and-prefix #'symbol-obfuscator)
(setf (ps-package-prefix '#:ps-test.obfuscate-and-prefix) "__FOO___")

(test-ps-js obfuscate-and-prefix
  (defun ps-test.obfuscate-and-prefix::xfun (a ps-test.obfuscate-and-prefix::b ps-test.my-library::d)
    (* a
       (ps-test.obfuscate-me::libfun2 ps-test.obfuscate-and-prefix::b a)
       (ps-test.my-library::library-function ps-test.my-library::d ps-test.obfuscate-and-prefix::b)))
  "function __FOO___ygvo(a, __FOO___c, my_library_d) {
    return a * mjcgvo3(__FOO___c, a) * my_library_libraryFunction(my_library_d, __FOO___c);
};")

(defpackage #:ps-test.pststpkg
  (:use #:parenscript))

(test namespace1 ()
  (setf (ps-package-prefix '#:ps-test.pststpkg) "prefix_")
  (is (string= "prefix_foo;" (normalize-js-code (ps* 'ps-test.pststpkg::foo)))))

(common-lisp:in-package #:ps-test.pststpkg)

(ps-test::test-ps-js namespace-and-special-forms
  (let ((foo (create bar 1 not-a-keyword something)))
    (return (and (not foo) (+ (getprop foo 'bar) some-other-var))))
"var prefix_foo = { prefix_bar : 1, prefix_notAKeyword : prefix_something };
return !prefix_foo && prefix_foo.prefix_bar + prefix_someOtherVar;")

(ps-test::test-ps-js exported-interface
  (defun ps-test:interface-function (baz)
    (+ baz ps-test.obfuscate-me::foo))
"function interfaceFunction(prefix_baz) {
    return prefix_baz + gpp;
};")
