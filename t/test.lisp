(in-package :js-test)

;; Testcases for parenscript

(defun trim-whitespace(str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun same-space-between-statements(code)
  (cl-ppcre:regex-replace-all "\\s*;\\s*" code (concatenate 'string (list #\; #\Newline))))

(defun no-indentation(code)
  (cl-ppcre:regex-replace-all (cl-ppcre:create-scanner "^\\s*" :multi-line-mode t) code ""))

(defun no-trailing-spaces(code)
  (cl-ppcre:regex-replace-all (cl-ppcre:create-scanner "\\s*$" :multi-line-mode t) code ""))

(defun normalize-js-code(str)
  (trim-whitespace (no-indentation (no-trailing-spaces (same-space-between-statements str)))))

(defmacro test-ps-js (testname parenscript javascript)
  `(test ,testname ()
    (setf js::*var-counter* 0)
    ;; is-macro expands its argument again when reporting failures, so
    ;; the reported temporary js-variables get wrong if we don't evalute first.
    (let ((generated-code (js-to-string ',parenscript))
          (js-code ,javascript))
      (is (string= (normalize-js-code generated-code)
                   (normalize-js-code js-code))))))

(defun run-tests()
  (format t "Running reference tests:~&")
  (run! 'ref-tests)
  (format t "Running other tests:~&")
  (run! 'ps-tests))

;;---------------------------------------------------------------------------
(def-suite ps-tests)
(in-suite ps-tests)

(test-ps-js plus-is-not-commutative
   (setf x (+ "before" x "after"))
   "x = 'before' + x + 'after'")

(test-ps-js plus-works-if-first
   (setf x (+ x "middle" "after"))
   "x += 'middle' + 'after'")

(test-ps-js setf-side-effects
            (progn
              (let ((x 10))
                (defun side-effect() 
                  (setf x 4)
                  (return 3))
                (setf x (+ 2 (side-effect) x 5))))
            "
var x = 10;
function sideEffect() {
  x = 4;
  return 3;
};
x = 2 + sideEffect() + x + 5;")
;; Parenscript used to optimize to much:
;;   var x = 10;
;;   function sideEffect() {
;;     x = 4;
;;     return 3;
;;   };
;;   x += 2 + sideEffect() + 5;
;;
;;   Which is 20, not 14


(test-ps-js dot-notation-bug
            (.match (+ "" x) "foo")
            "('' + x).match('foo')")

(test-ps-js method-call-op-form (.to-string (+ "" x)) "('' + x).toString()")
(test-ps-js method-call-number (.to-string 10) "(10).toString()")
(test-ps-js method-call-string (.to-string "hi") "'hi'.toString()")
(test-ps-js method-call-lit-object
            (.to-string (create :to-string : (lambda ()
                                                (return "it works"))))
            "({ toString : function () {
        return 'it works';
      } }).toString()")

(test-ps-js method-call-variable
            (.to-string x)
            "x.toString()")

(test-ps-js method-call-array
            (.to-string (list 10 20))
            "[10, 20].toString()")
(test-ps-js method-call-fn-call
            (.to-string (foo))
            "foo().toString()")
(test-ps-js method-call-lambda-fn
            (.to-string (lambda () (alert 10)))
            "(function () {alert(10);}).toString()")
(test-ps-js method-call-lambda-call
            (.to-string ((lambda (x) (return x)) 10))
            "(function (x) {return x;})(10).toString()")

(test no-whitespace-before-dot
  (let* ((str (js:js* '(.to-string ((lambda (x) (return x)) 10))))
         (dot-pos (position #\. str :test #'char=))
         (char-before (elt str (1- dot-pos)))
         (a-parenthesis #\)))
    (is (char= char-before a-parenthesis))))

;; A problem with long nested operator, when the statement spanned several rows
;; the rows would not be joined together correctly.
(test-ps-js bug-dwim-join
   (alert (html ((:div :id 777
                       :style (css-inline :border "1pxsssssssssss"
                                          :font-size "x-small"
                                          :height (* 2 200)
                                          :width (* 2 300))))))
   "alert
('<div id=\"777\" style=\"'
 + ('border:1pxsssssssssss;font-size:x-small;height:' + 2 * 200 + ';width:'
 + 2 * 300)
 + '\"></div>')") ;";This line should start with a plus character.


(test-ps-js simple-slot-value
  (let ((foo (create :a 1)))
   (alert (slot-value foo 'a)))
  "{
    var foo = { a : 1 };
    alert(foo.a);
   }")

(test-ps-js buggy-slot-value
   (let ((foo (create :a 1))
        (slot-name "a"))
    (alert (slot-value foo slot-name)))
  "{
    var foo = { a : 1 };
    var slotName = 'a';
    alert(foo[slotName]);
   }"); Last line was alert(foo.slotName) before bug-fix.

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
         case 1:   ;
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
                   (#\f . #\Form)
                   ("u000b" . ,(code-char #x000b));;Vertical tab, too uncommon to bother with
                   (#\n . #\Newline)
                   (#\r . #\Return)
                   (#\' . #\');;Double quote need not be quoted because parenscript strings are single quoted
                   (#\t . #\Tab)
                   ("u001f" . ,(code-char #x001f));; character below 32
                   ("u0080" . ,(code-char 128)) ;;Character over 127. Actually valid, parenscript escapes them to be sure.
                   ("uabcd" . ,(code-char #xabcd)))));; Really above ascii.
    (loop for (js-escape . lisp-char) in escapes
          for generated = (js-to-string `(let ((x , (format nil "hello~ahi" lisp-char)))))
          for wanted = (format nil "{
  var x = 'hello\\~ahi';
}" js-escape)
          do (is (string= generated wanted)))))
