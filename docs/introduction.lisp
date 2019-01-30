(in-package :ps)

;;; Introduction
;;;
;;; Parenscript is a language that looks a lot like Common Lisp, but
;;; is JavaScript in disguise. This way, JavaScript programs can be
;;; seamlessly integrated in a Lisp web application. The programmer
;;; doesn't have to resort to a different syntax, and JavaScript code
;;; can easily be generated without having to resort to complicated
;;; string generation or `FORMAT' expressions.
;;;
;;; The following Lisp expression is a call to the Parenscript
;;; translator Parenscript transforms the expression in Parenscript
;;; into an equivalent, human-readable expression in JavaScript.

(ps
  (defun foobar (a b)
    (+ a b)))

;;; The resulting javascript is:

"
function foobar(a, b) {
  return a + b;
}
"

;;; Great care has been given to the indentation and overall
;;; readability of the generated JavaScript code.

;;; Features
;;;
;;; Parenscript supports all the statements and expressions defined by
;;; the EcmaScript 262 standard. Lisp symbols are converted to
;;; camelcase, javascript-compliant syntax. This idea is taken from
;;; Linj by Antonio Menezes Leitao. Case sensitivity (using the
;;; :invert readtable-case option) is supported. Here are a few
;;; examples of Lisp symbol to JavaScript name conversion:

(ps foobar)      => "foobar"
(ps foo-bar)     => "fooBar"
(ps foo-b@r)     => "fooBatr"
(ps *array)      => "Array"
(ps FooBar)      => "FooBar"

;;; Parenscript supports a subset of Common Lisp iteration constructs.
;;; `for' loops can be written using the customary `DO*' syntax.

(ps
  (do* ((i 0 (incf i))
        (j (aref arr i) (aref arr i)))
       ((>= i 10))
    (alert (+ "i is " i " and j is " j))))

;; compiles to
"
for (var i = 0, j = arr[i]; i < 10; i = ++i, j = arr[i]) {
  alert('i is ' + i + ' and j is ' + j);
};
"
;;; Parenscript uses the Lisp reader, allowing for reader macros. It
;;; also comes with its own macro environment, allowing host Lisp
;;; macros and Parenscript macros to coexist without interfering with
;;; each other.  For example, the `1+' construct is implemented using
;;; a Parenscript macro:

(defpsmacro 1+ (form)
  `(+ ,form 1))

;;; Parenscript allows the creation of JavaScript objects in a Lispy
;;; way, using keyword arguments.

(ps
  (create :foo "foo"
          :bla "bla"))

;; compiles to
"
{ foo : 'foo',
  bla : 'bla' }
"
;;; Parenscript features a HTML generator. Using the same syntax as
;;; the HTMLGEN package of Franz, Inc., it can generate JavaScript
;;; string expressions. This allows for a clean integration of HTML in
;;; Parenscript code, instead of writing the tedious and error-prone
;;; string generation code generally found in JavaScript.

(ps
  (defun add-div (name href link-text)
    (funcall (getprop document 'write)
             (ps-html ((:div :id name)
                       "The link is: "
                       ((:a :href href) link-text))))))

;; compiles to
"
function addDiv(name, href, linkText) {
    return document.write(['<div id=\\\"', name, '\\\">The link is: <a href=\\\"', href, '\\\">', linkText, '</a></div>'].join(''));
};
"
