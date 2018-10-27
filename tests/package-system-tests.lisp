;;; Copyright 2007 Red Daly
;;; Copyright 2007 Vladimir Sedach

;;; SPDX-License-Identifier: BSD-3-Clause

;;; Redistribution and use in source and binary forms, with or
;;; without modification, are permitted provided that the following
;;; conditions are met:

;;; 1. Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.

;;; 2. Redistributions in binary form must reproduce the above
;;; copyright notice, this list of conditions and the following
;;; disclaimer in the documentation and/or other materials provided
;;; with the distribution.

;;; 3. Neither the name of the copyright holder nor the names of its
;;; contributors may be used to endorse or promote products derived
;;; from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

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

(test-ps-js lib-function2
  (defun parenscript.tests.my-library::library-function
      (parenscript.tests.my-library::x
       &key ((:y parenscript.tests.my-library::z) 1))
    (+ parenscript.tests.my-library::x parenscript.tests.my-library::z))
  "function my_library_libraryFunction(my_library_x) {
    var _js2 = arguments.length;
    for (var n1 = 1; n1 < _js2; n1 += 2) {
        switch (arguments[n1]) {
        case 'y':
            my_library_z = arguments[n1 + 1];
        };
    };
    var my_library_z = 'undefined' === typeof my_library_z ? 1 : my_library_z;

    return my_library_x + my_library_z;
};")

(test-ps-js uniform-symbol-handling1
  (progn (create parenscript.tests.my-library::foo 1)
         (getprop foo 'parenscript.tests.my-library::foo))
  "{ my_library_foo : 1 };
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
  "{ prefix_x : 1 + 2 };")

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
