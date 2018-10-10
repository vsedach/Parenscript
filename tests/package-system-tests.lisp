;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:parenscript.tests)

(fiveam:in-suite package-system-tests)

(test-ps-js operator-packages1
  (#:new)
  "new();")

(defpackage #:parenscript.tests.my-library
  (:use #:parenscript))
(setf (ps-package-prefix '#:parenscript.tests.my-library)
      "my_library_")

(test-ps-js lib-function1
  (defun parenscript.tests.my-library::library-function (x y)
    (+ x y))
  "function my_library_libraryFunction(x, y) {
        return x + y;
     };")

(test-ps-js uniform-symbol-handling1
  (progn (create parenscript.tests.my-library::foo 1)
         (getprop foo 'parenscript.tests.my-library::foo))
  "({ my_library_foo : 1 });
foo.my_library_foo;")

(let ((map (make-hash-table)))
  (defun symbol-obfuscator (symbol)
    (or #1=(gethash symbol map)
        (setf #1# (make-symbol (map 'string (lambda (x)
                                              (code-char (1+ (char-code x))))
                                    (symbol-name symbol)))))))

(defpackage #:parenscript.tests.obfuscate-me)
(obfuscate-package '#:parenscript.tests.obfuscate-me
                   #'symbol-obfuscator)

(test-ps-js obfuscation1
  (defun parenscript.tests.obfuscate-me::libfun2 (a b parenscript.tests.obfuscate-me::foo)
    (+ a (parenscript.tests.my-library::library-function b parenscript.tests.obfuscate-me::foo)))
  "function mjcgvo3(a, b, gpp) {
    return a + my_library_libraryFunction(b, gpp);
};")

(defpackage #:parenscript.tests.obfuscate-and-prefix)
(obfuscate-package '#:parenscript.tests.obfuscate-and-prefix #'symbol-obfuscator)
(setf (ps-package-prefix '#:parenscript.tests.obfuscate-and-prefix) "__FOO___")

(test-ps-js obfuscate-and-prefix
  (defun parenscript.tests.obfuscate-and-prefix::xfun (a parenscript.tests.obfuscate-and-prefix::b parenscript.tests.my-library::d)
    (* a
       (parenscript.tests.obfuscate-me::libfun2 parenscript.tests.obfuscate-and-prefix::b a)
       (parenscript.tests.my-library::library-function parenscript.tests.my-library::d parenscript.tests.obfuscate-and-prefix::b)))
  "function __FOO___ygvo(a, __FOO___c, my_library_d) {
    return a * mjcgvo3(__FOO___c, a) * my_library_libraryFunction(my_library_d, __FOO___c);
};")

(defpackage #:parenscript.tests.pststpkg
  (:use #:parenscript))

(setf (ps-package-prefix '#:parenscript.tests.pststpkg) "prefix_")

(fiveam:test namespace1 ()
  (fiveam:is (string=
              (ps* 'parenscript.tests.pststpkg::foo)
              "prefix_foo;")))

(cl:in-package #:parenscript.tests.pststpkg)

(parenscript.tests::test-ps-js namespace-and-special-forms
  (defun foo ()
    (let ((foo (create bar 1 not-a-keyword something)))
      (return-from foo (and (not foo) (+ (getprop foo 'bar) some-other-var)))))
"function prefix_foo() {
var foo1 = { prefix_bar : 1, prefix_notAKeyword : prefix_something };
return !foo1 && foo1.prefix_bar + prefix_someOtherVar;
};")

(parenscript.tests::test-ps-js exported-interface
  (defun parenscript.tests:interface-function (baz)
    (+ baz parenscript.tests.obfuscate-me::foo))
"function interfaceFunction(prefix_baz) {
    return prefix_baz + gpp;
};")

(parenscript.tests::test-ps-js prefixed-symbol-macro-obj1
  (symbol-macrolet ((x (+ 1 2)))
    (ps:create x x))
  "({ prefix_x : 1 + 2 });")

(cl:in-package #:parenscript.tests)

(fiveam:test compile-stream-in-package
  (fiveam:is
   (string=
    (with-input-from-string (s "
(defun parenscript.tests.obfuscate-and-prefix::xfun (a parenscript.tests.obfuscate-and-prefix::b parenscript.tests.my-library::d)
    (* a
       (parenscript.tests.obfuscate-me::libfun2 parenscript.tests.obfuscate-and-prefix::b a)
       (parenscript.tests.my-library::library-function parenscript.tests.my-library::d parenscript.tests.obfuscate-and-prefix::b)))

(in-package #:parenscript.tests.pststpkg)

(defun parenscript.tests:interface-function (baz)
    (+ baz parenscript.tests.obfuscate-me::foo))
")
         (ps-compile-stream s))
       "function __FOO___ygvo(a, __FOO___c, my_library_d) {
    return a * mjcgvo3(__FOO___c, a) * my_library_libraryFunction(my_library_d, __FOO___c);
};
function interfaceFunction(prefix_baz) {
    return prefix_baz + gpp;
};
")))
