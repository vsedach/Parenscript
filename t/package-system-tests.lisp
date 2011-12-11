;; Copying and distribution of this file, with or without
;; modification, are permitted in any medium without royalty. This
;; file is offered as-is, without any warranty.

(in-package #:ps-test)

(in-suite package-system-tests)

(test-ps-js operator-packages1
  (#:new)
  "new();")

(defpackage #:ps-test.my-library
  (:use #:parenscript))
(setf (ps-package-prefix '#:ps-test.my-library) "my_library_")

(test-ps-js lib-function1
  (defun ps-test.my-library::library-function (x y)
    (+ x y))
  "function my_library_libraryFunction(x, y) {
        return x + y;
     };")

(test-ps-js uniform-symbol-handling1
  (progn (create ps-test.my-library::foo 1)
         (getprop foo 'ps-test.my-library::foo))
  "({ my_library_foo : 1 });
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

(setf (ps-package-prefix '#:ps-test.pststpkg) "prefix_")

(test namespace1 ()
  (is (string= "prefix_foo;" (normalize-js-code (ps* 'ps-test.pststpkg::foo)))))

(cl:in-package #:ps-test.pststpkg)

(ps-test::test-ps-js namespace-and-special-forms
  (defun foo ()
    (let ((foo (create bar 1 not-a-keyword something)))
      (return-from foo (and (not foo) (+ (getprop foo 'bar) some-other-var)))))
"function prefix_foo() {
var foo1 = { prefix_bar : 1, prefix_notAKeyword : prefix_something };
return !foo1 && foo1.prefix_bar + prefix_someOtherVar;
};")

(ps-test::test-ps-js exported-interface
  (defun ps-test:interface-function (baz)
    (+ baz ps-test.obfuscate-me::foo))
"function interfaceFunction(prefix_baz) {
    return prefix_baz + gpp;
};")

(cl:in-package #:ps-test)

(test compile-stream-in-package
  (is (string=
       "function __FOO___ygvo(a, __FOO___c, my_library_d) {
    return a * mjcgvo3(__FOO___c, a) * my_library_libraryFunction(my_library_d, __FOO___c);
};
function interfaceFunction(prefix_baz) {
    return prefix_baz + gpp;
};
"
       (with-input-from-string (s "
(defun ps-test.obfuscate-and-prefix::xfun (a ps-test.obfuscate-and-prefix::b ps-test.my-library::d)
    (* a
       (ps-test.obfuscate-me::libfun2 ps-test.obfuscate-and-prefix::b a)
       (ps-test.my-library::library-function ps-test.my-library::d ps-test.obfuscate-and-prefix::b)))

(in-package #:ps-test.pststpkg)

(defun ps-test:interface-function (baz)
    (+ baz ps-test.obfuscate-me::foo))
")
         (ps-compile-stream s)))))
