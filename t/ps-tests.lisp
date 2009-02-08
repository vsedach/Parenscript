(in-package :ps-test)

;;; Hand-written unit tests

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite ps-tests))
(in-suite ps-tests)

(test-ps-js plus-is-not-commutative
  (setf x (+ "before" x "after"))
  "x = 'before' + x + 'after';")

(test-ps-js plus-works-if-first
  (setf x (+ x "middle" "after"))
  "x += 'middle' + 'after';")

(test-ps-js setf-side-effects
  (progn
    (let* ((x 10))
      (defun side-effect() 
        (setf x 4)
        (return 3))
      (setf x (+ 2 (side-effect) x 5))))
  "var x = 10;
function sideEffect() {
  x = 4;
  return 3;
};
x = 2 + sideEffect() + x + 5;")
;; Parenscript used to optimize incorrectly:
;;   var x = 10;
;;   function sideEffect() {
;;     x = 4;
;;     return 3;
;;   };
;;   x += 2 + sideEffect() + 5;
;;
;;   Which is 20, not 14


(test-ps-js method-call-op-form
  ((@ (+ "" x) to-string))
  "('' + x).toString()")

(test-ps-js method-call-op-form-args
  ((@ (+ "" x) to-string) 1 2 :baz 3)
  "('' + x).toString(1, 2, { baz : 3 })")

(test-ps-js method-call-number
  ((@ 10 to-string))
  "( 10 ).toString()")

(test-ps-js method-call-string
  ((@ "hi" to-string))
  "'hi'.toString()")

(test-ps-js method-call-lit-object
  ((@ (create :to-string (lambda () (return "it works"))) to-string))
  "( { toString : function () { return 'it works'; } } ).toString()")

(test-ps-js method-call-conditional
  ((if a x y) 1)
  "(a ? x : y)(1)")

(test-ps-js method-call-variable
  ((@ x to-string))
  "x.toString()")

(test-ps-js method-call-array
  ((@ (list 10 20) to-string))
  "[ 10, 20 ].toString()")

(test-ps-js method-call-fn-call
  ((@ (foo) to-string))
  "foo().toString()")

(test-ps-js method-call-lambda-fn
  ((@ (lambda () (alert 10)) to-string))
  "( function () { alert(10); } ).toString()")

(test-ps-js method-call-lambda-call
  ((@ ((lambda (x) (return x)) 10) to-string))
  "(function (x) { return x; })(10).toString()")

(test no-whitespace-before-dot
  (let* ((str (ps1* '((@ ((lambda (x) (return x)) 10) to-string))))
         (dot-pos (position #\. str :test #'char=))
         (char-before (elt str (1- dot-pos)))
         (a-parenthesis #\)))
    (is (char= char-before a-parenthesis))))

(test-ps-js simple-slot-value
  (let* ((foo (create :a 1)))
    (alert (slot-value foo 'a)))
  "var foo = { a : 1 };
   alert(foo.a);")

(test-ps-js buggy-slot-value
   (let* ((foo (create :a 1))
          (slot-name "a"))
    (alert (slot-value foo slot-name)))
  " var foo = { a : 1 };
    var slotName = 'a';
    alert(foo[slotName]);
   "); Last line was alert(foo.slotName) before bug-fix.

(test-ps-js buggy-slot-value-two
  (slot-value foo (get-slot-name))
  "foo[getSlotName()]")

(test-ps-js old-case-is-now-switch
  ;; Switch was "case" before, but that was very non-lispish.
  ;; For example, this code makes three messages and not one
  ;; which may have been expected. This is because a switch
  ;; statment must have a break statement for it to return
  ;; after the alert. Otherwise it continues on the next
  ;; clause.
  (switch (aref blorg i)
     (1 (alert "one"))
     (2 (alert "two"))
     (default (alert "default clause")))
     "switch (blorg[i]) {
         case 1:   alert('one');
         case 2:   alert('two');
         default:   alert('default clause');
         }")

(test-ps-js lisp-like-case
   (case (aref blorg i)
     (1 (alert "one"))
     (2 (alert "two"))
     (default (alert "default clause")))    
     "switch (blorg[i]) {
         case 1:
                   alert('one');
                   break;
         case 2:
                   alert('two');
                   break;
         default:   alert('default clause');
         }")


(test-ps-js even-lispier-case
  (case (aref blorg i)
      ((1 2) (alert "Below three"))
      (3 (alert "Three"))
      (t (alert "Something else")))
   "switch (blorg[i]) {
         case 1:
         case 2:
                   alert('Below three');
                   break;
         case 3:
                   alert('Three');
                   break;
         default:   alert('Something else');
    }")

(test-ps-js otherwise-case
   (case (aref blorg i)
     (1 (alert "one"))
     (otherwise (alert "default clause")))
     "switch (blorg[i]) {
         case 1:
                   alert('one');
                   break;
         default:   alert('default clause');
         }")

(test escape-sequences-in-string
  (let ((escapes `((#\\ . #\\)
                   (#\b . #\Backspace)
                   (#\f . ,(code-char 12))
                   ("u000B" . ,(code-char #x000b));;Vertical tab, too uncommon to bother with
                   (#\n . #\Newline)
                   (#\r . #\Return)
                   (#\' . #\');;Double quote need not be quoted because parenscript strings are single quoted
                   (#\t . #\Tab)
                   ("u001F" . ,(code-char #x001f));; character below 32
                   ("u0080" . ,(code-char 128)) ;;Character over 127. Actually valid, parenscript escapes them to be sure.
                   ("uABCD" . ,(code-char #xabcd)))));; Really above ascii.
    (loop for (js-escape . lisp-char) in escapes
          for generated = (ps1* `(let* ((x ,(format nil "hello~ahi" lisp-char)))))
          for wanted = (format nil "var x = 'hello\\~ahi';" js-escape)
          do (is (string= (normalize-js-code generated) wanted)))))
  
(test-ps-js complicated-symbol-name1
  grid-rows[foo].bar
  "gridRows[foo].bar")

(test-ps-js complicated-symbol-name2
  *grid-rows*[foo].bar
  "GRIDROWS[foo].bar")

(test-ps-js slot-value-setf
  (setf (slot-value x 'y) (+ (+ a 3) 4))
  "x.y = (a + 3) + 4;")

(test-ps-js slot-value-conditional1
  (slot-value (if zoo foo bar) 'x)
  "(zoo ? foo : bar).x")

(test-ps-js slot-value-conditional2
  (slot-value (if (not zoo) foo bar) 'x)
  "(!zoo ? foo : bar).x")

(test script-star-eval1
  (is (string= "x = 1; y = 2;" (normalize-js-code (ps* '(setf x 1) '(setf y 2))))))

(test script-star-eval2
  (is (string= "x = 1;" (normalize-js-code (ps* '(setf x 1))))))

(test-ps-js slot-value-null1
  (slot-value foo nil)
  "foo")

(test-ps-js slot-value-null2
  (slot-value foo 'nil)
  "foo")

(test-ps-js unquoted-nil
  nil
  "null")

(test-ps-js list-with-single-nil
  (array 'nil)
  "[null]")

(test-ps-js quoted-nil
  'nil
  "null")

(test defsetf1
  (ps (defsetf baz (x y) (newval) `(set-baz ,x ,y ,newval)))
  (is (string= "var _js2 = 1; var _js3 = 2; var _js1 = 3; setBaz(_js2, _js3, _js1);"
               (normalize-js-code (let* ((ps:*ps-gensym-counter* 0))
                                    (ps (setf (baz 1 2) 3)))))))

(test defsetf-short
  (ps (defsetf baz set-baz "blah"))
  (is (string= "setBaz(1, 2, 3, 'foo');" (normalize-js-code (ps (setf (baz 1 2 3) "foo"))))))

(test defun-setf1
  (is (and (string= (normalize-js-code (ps:ps (defun (setf some-thing) (new-val i1 i2)
                                                (setf (aref *some-thing* i1 i2) new-val))))
               "function __setf_someThing(newVal, i1, i2) { SOMETHING[i1][i2] = newVal; };")
           (string= (normalize-js-code (ps:ps-doc (setf (some-thing 1 2) "foo")))
                    "var _js2 = 1; var _js3 = 2; var _js1 = 'foo'; __setf_someThing(_js1, _js2, _js3);"))))

(test-ps-js defun-optional1
  (defun test-opt (&optional x) (return (if x "yes" "no")))
  "function testOpt(x) {
    if (x === undefined) {
        x = null;
    };
    return x ? 'yes' : 'no';
}")

(test-ps-js defun-optional2
  (defun foo (x &optional y) (+ x y))
  "function foo(x, y) {
    if (y === undefined) {
        y = null;
    };
    x + y;
}")

(test-ps-js defun-optional3
  (defun blah (&optional (x 0))
    (return x))
  "function blah(x) {
    if (x === undefined) {
        x = 0;
    };
    return x;
}")

(test-ps-js return-nothing
  (return)
  "return null")

(test-ps-js set-timeout
  (do-set-timeout (10) (alert "foo"))
  "setTimeout(function () { alert('foo'); }, 10)")
(test-ps-js operator-precedence
  (* 3 (+ 4 5) 6)
  "3 * (4 + 5) * 6")

(test-ps-js operators-1
  (in prop obj)
  "prop in obj")

(test-ps-js incf1
  (incf foo bar)
  "foo += bar")

(test-ps-js decf1
  (decf foo bar)
  "foo -= bar")

(test-ps-js incf2
  (incf x 5)
  "x += 5")

(test-ps-js decf2
  (decf y 10)
  "y -= 10")

(test-ps-js setf-conditional
  (setf foo (if x 1 2))
  "foo = x ? 1 : 2;")

(test-ps-js obj-literal-numbers
  (create 1 "foo")
  "{ 1 : 'foo' }")

(test-ps-js obj-literal-strings
  (create "foo" 2)
  "{ 'foo' : 2 }")

(test-ps-js slot-value-string
  (slot-value foo "bar")
  "foo['bar']")

(test-ps-js slot-value-string1
  (slot-value "bar" 'length)
  "'bar'.length")

(test-ps-js slot-value-progn
  (slot-value (progn (some-fun "abc") "123") "length")
  "(someFun('abc'), '123')['length']")

(test-ps-js method-call-block
  ((@ (progn (some-fun "abc") "123") to-string))
  "(someFun('abc'), '123').toString()")

(test-ps-js create-blank
  (create)
  "{ }")

(test-ps-js blank-object-literal
  {}
  "{ }")

(test-ps-js array-literal1
  []
  "[]")

(test-ps-js array-literal2
  ([])
  "[]")

(test-ps-js array-literal3
  ([] 1 2 3)
  "[1, 2, 3]")

(test-ps-js array-literal4
  ([] 1 (2 3))
  "[1, [2, 3]]")

(test-ps-js array-literal5
  ([] (1 2) ("a" "b"))
  "[[1, 2], ['a', 'b']]")

(test-ps-js defun-rest1
  (defun foo (&rest bar) (alert bar[1]))
  "function foo() {
    var bar = [];
    for (var i2 = 0; i2 < arguments.length - 0; i2 += 1) {
        bar[i2] = arguments[i2 + 0];
    };
    alert(bar[1]);
}")

(test-ps-js defun-rest2
  (defun foo (baz &rest bar) (return (+ baz (aref bar 1))))
  "function foo(baz) {
    var bar = [];
    for (var i2 = 0; i2 < arguments.length - 1; i2 += 1) {
        bar[i2] = arguments[i2 + 1];
    };
    return baz + bar[1];
}")

(test-ps-js defun-keyword1
  (defun zoo (foo bar &key baz) (return (+ foo bar baz)))
  "function zoo(foo, bar, _js1) {
    if (_js1 === undefined) {
        _js1 = {  };
    };
    return foo + bar + _js1.baz;
}")

(test-ps-js defun-keyword2
  (defun zoo (&key baz) (return (* baz baz)))
  "function zoo(_js1) {
    if (_js1 === undefined) {
        _js1 = {  };
    };
    return _js1.baz * _js1.baz;
}")

(test-ps-js defun-keyword3
  (defun zoo (&key baz (bar 4)) (return (* baz bar)))
  "function zoo(_js1) {
    if (_js1 === undefined) {
        _js1 = {  };
    };
    if (_js1.bar === undefined) {
        _js1.bar = 4;
    };
    return _js1.baz * _js1.bar;
}")

(test-ps-js keyword-funcall1
  (func :baz 1)
  "func({ baz : 1 })")

(test-ps-js keyword-funcall2
  (func :baz 1 :bar foo)
  "func({ baz : 1, bar : foo })")

(test-ps-js keyword-funcall3
  (fun a b :baz c)
  "fun(a, b, { baz : c })")
  
(test-ps-js cond1
  (cond ((= x 1) 1))
  "if (x == 1) {
    1;
}")

(test-ps-js cond2
  (cond ((= x 1) 2) ((= y (* x 4)) (foo "blah") (* x y)))
  "if (x == 1) {
    2;
} else if (y == x * 4) {
    foo('blah');
    x * y;
}")

(test-ps-js if-exp-without-else-returns-null
  (return (if x 1))
  "return x ? 1 : null")

(test-ps-js progn-expression-single-statement
  (return (progn (* x y)))
  "return x * y")

(test-ps-js cond-expression1
  (defun foo () (return (cond ((< 1 2) (bar "foo") (* 4 5)))))
  "function foo() {
    return 1 < 2 ? (bar('foo'), 4 * 5) : null;
}")

(test-ps-js cond-expression2
  (defun foo () (return (cond ((< 2 1) "foo") ((= 7 7) "bar"))))
  "function foo() {
    return 2 < 1 ? 'foo' : (7 == 7 ? 'bar' : null);
}")

(test-ps-js cond-expression-final-t-clause
  (defun foo () (return (cond ((< 1 2) (bar "foo") (* 4 5)) ((= a b) (+ c d)) ((< 1 2 3 4 5) x) (t "foo"))))
  "function foo() {
    return 1 < 2 ? (bar('foo'), 4 * 5) : (a == b ? c + d : (1 < 2 < 3 < 4 < 5 ? x : 'foo'));
}")

(test-ps-js cond-expression-middle-t-clause ;; should this signal a warning?
  (defun foo () (return (cond ((< 2 1) 5) (t "foo") ((< 1 2) "bar"))))
  "function foo() {
    return 2 < 1 ? 5 : 'foo';
}")

(test-ps-js funcall-if-expression
  (document.write
   (if (= *linkornot* 1)
       (ps-html ((:a :href "#"
                     :onclick (ps-inline (transport)))
                 img))
       img))
  "document.write(LINKORNOT == 1 ? '<A HREF=\"#\" ONCLICK=\"' + ('javascript:' + 'transport()') + '\">' + img + '</A>' : img)")

(test-ps-js negate-number-literal ;; ok, this was broken and fixed before, but no one bothered to add the test!
  (- 1)
  "-1")

(test macro-environment1
  (is (string= (normalize-js-code (let* ((macroname (gensym)))
                                    (ps* `(defmacro ,macroname (x) `(+ ,x 123))
                                         `(defun test1 ()
                                           (macrolet ((,macroname (x) `(aref data ,x)))
                                             (when (,macroname x)
                                               (setf (,macroname x) 123)))))))
               (normalize-js-code
"function test1() {
    if (data[x]) {
        data[x] = 123;
    };
};
"))))

(test macro-environment2
  (is (string= (normalize-js-code (let ((outer-lexical-variable 1))
                                    (defpsmacro macro-environment2-macro (x)
                                      `(+ ,outer-lexical-variable ,x))
                                    (ps* '(macro-environment2-macro 2))))
               (normalize-js-code "1 + 2;"))))

(test-ps-js ampersand-whole-1
  (macrolet ((foo (&whole foo bar baz)
               (declare (ignore bar baz))
               (format nil "~a" foo)))
    (foo 1 2))
  "'(FOO 1 2)';")

(test-ps-js keyword-consistent
  :x
  "'x'")

(test-ps-js simple-symbol-macrolet
  (symbol-macrolet ((x 1)) x)
  "1;")

(test-ps-js compound-symbol-macrolet
  (symbol-macrolet ((x 123)
                    (y (* 2 x)))
     y)
  "2 * 123;")

(test-ps-js define-symbol-macro
  (progn (define-symbol-macro tst-sym-macro 2)
         tst-sym-macro)
  "2;")

(test-ps-js expression-progn
  (defun f () (return (progn (foo) (if x 1 2))))
  "function f() {
    return (foo(), x ? 1 : 2);
}")

(test-ps-js let-decl-in-expression
  (defun f (x) (return (if x 1 (let* ((foo x)) foo))))
  "function f(x) {
    var foo;
    return x ? 1 : (foo = x, foo);
}")

(test-ps-js special-var1
  (progn (defvar *foo*) (let* ((*foo* 2)) (* *foo* 2)))
  "var FOO;
var tempstackvar1;
try {
    tempstackvar1 = FOO;
    FOO = 2;
    FOO * 2;
} finally {
    FOO = tempstackvar1;
    delete tempstackvar1;
};")

(test-ps-js special-var2
  (progn (defvar *foo*) (let* ((*baz* 3) (*foo* 2)) (* *foo* 2 *baz*)))
  "var FOO;
var BAZ = 3;
var tempstackvar1;
try {
    tempstackvar1 = FOO;
    FOO = 2;
    FOO * 2 * BAZ;
} finally {
    FOO = tempstackvar1;
    delete tempstackvar1;
};
")

(test-ps-js literal1
  (setf x undefined)
  "x = undefined;")

(test-ps-js literal2
  (aref this x)
  "this[x]")

(test-ps-js setf-dec1
  (setf x (- 1 x 2))
  "x = 1 - x - 2;")

(test-ps-js setf-dec2
  (setf x (- x 1 2))
  "x = x - 1 - 2;")

(test-ps-js special-char-equals
  blah=
  "blahequals")

(test-ps-js setf-operator-priority
  (return (or (slot-value cache id)
              (setf (slot-value cache id) (document.get-element-by-id id))))
  "return cache[id] || (cache[id] = document.getElementById(id))")

(test-ps-js aref-operator-priority
  (aref (if (and x (> (length x) 0))
            (aref x 0)
            y)
        z)
  "(x && x.length > 0 ? x[0] : y)[z]")

(test-ps-js aref-operator-priority1
  (aref (or (slot-value x 'y)
            (slot-value a 'b))
        z)
  "(x.y || a.b)[z]")

(test-ps-js aref-operator-priority2
  (aref (if a b c) 0)
  "(a ? b : c)[0]")

(test-ps-js negative-operator-priority
  (- (if x y z))
  "-(x ? y : z)")

(test-ps-js op-p1
  (new (or a b))
  "new (a || b)")

(test-ps-js op-p2
  (delete (if a (or b c) d))
  "delete (a ? b || c : d)")

(test-ps-js op-p3
  (not (if (or x (not y)) z))
  "!(x || !y ? z : null)")

(test-ps-js op-p4
  (- (- (* 1 2) 3))
  "-(1 * 2 - 3)")

(test-ps-js op-p5
  (instanceof (or a b) (if x y z))
  "((a || b) instanceof (x ? y : z))")

(test-ps-js op-p6
  (doeach (x (or a b)))
  "for (var x in (a || b)) { };")

(test-ps-js op-p7
  (or x (if (= x 0) "zero" "empty"))
  "x || (x == 0 ? 'zero' : 'empty')")

(test-ps-js named-op-expression
  (throw (if a b c))
  "throw a ? b : c")

(test-ps-js named-op-expression1
  (typeof (or x y))
  "typeof (x || y)")

(test-ps-js aref-array-expression
  (aref (or a b c) 0)
  "(a || b || c)[0]")

(test-ps-js slot-value-operator
  (slot-value (or a b c) 'd)
  "(a || b || c).d")

(test-ps-js slot-value-parens
  (slot-value (slot-value foo 'bar) 'baz)
  "foo.bar.baz")

(test-ps-js funcall-funcall
  ((foo))
  "foo()()")

(test-ps-js expression-funcall
  ((or (@ window eval) eval) foo nil)
  "(window.eval || eval)(foo, null)")

(test-ps-js expression-funcall1
  (((or (@ window eval) eval) foo nil))
  "(window.eval || eval)(foo, null)()")

(test-ps-js expression-funcall2
  (((or (@ window eval) eval)) foo nil)
  "(window.eval || eval)()(foo, null)")

(test-ps-js slot-value-object-literal
  (slot-value (create :a 1) 'a)
  "({ a : 1 }).a")

(test-ps-js slot-value-lambda
  (slot-value (lambda ()) 'prototype)
  "(function () { }).prototype")

(test-ps-js who-html1
  (who-ps-html (:span :class "ticker-symbol"
                      :ticker-symbol symbol
                      (:a :href "http://foo.com"
                          symbol)
                      (:span :class "ticker-symbol-popup")))
  "'<SPAN CLASS=\"ticker-symbol\" TICKER-SYMBOL=\"' + symbol + '\"><A HREF=\"http://foo.com\">' + symbol + '</A><SPAN CLASS=\"ticker-symbol-popup\"/></SPAN>'")

(test-ps-js flet1
  ((lambda () (flet ((foo (x) (return (1+ x)))) (return (foo 1)))))
  "(function () {
    var foo = function (x) {
        return x + 1;
    };
    return foo(1);
})()")

(test-ps-js labels1
  ((lambda () (labels ((foo (x) 
                         (return (if (=== 0 x)
                                     0
                                     (+ x (foo (1- x)))))))
                (return (foo 3)))))
  "(function () {
    var foo = function foo(x) {
        return 0 === x ? 0 : x + foo(x - 1);
    };
    return foo(3);
})()")

(test-ps-js for-loop-var-init-exp
  ((lambda (x)
     (return (do* ((y (if x 0 1) (1+ y))
                   (z 0 (1+ z)))
                  ((= y 3) z))))
   true)
  "(function (x) {
    return (function () {
        for (var y = x ? 0 : 1, z = 0; y != 3; y += 1, z += 1) {
        };
        return z;
    })();
})(true)")
