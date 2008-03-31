(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *parenscript-lang-exports*
    '(
      ;; literals
      #:t
      #:f
      #:true
      #.(symbol-name 'nil) ; for case-sensitive Lisps like some versions of Allegro
      #:this
      #:false
      #:undefined

      ;; keywords
      #:break
      #:continue

      ;; array literals
      #:array
      #:list
      #:aref
      #:make-array

      ;; operators
      #:! #:not #:~
      #:* #:/ #:%
      #:+ #:-
      #:<< #:>>
      #:>>>
      #:< #:> #:<= #:>=
      #:in
      #:eql #:== #:!= #:=
      #:=== #:!==
      #:&
      #:^
      #:\|
      #:\&\& #:and
      #:\|\| #:or
      #:>>= #:<<=
      #:*= #:/= #:%= #:+= #:\&= #:^= #:\|= #:~=
      #:1+ #:1-
      #:incf #:decf

      ;; body forms
      #:progn

      ;; object literals
      #:create
      #:with-slots

      ;; macros
      #:macrolet
      #:symbol-macrolet

      ;; if
      #:if
      #:when
      #:unless

      ;; single argument statements
      #:return
      #:throw

      ;; single argument expressions
      #:delete
      #:void
      #:typeof
      #:instanceof
      #:new

      ;; assignment and binding
      #:setf
      #:defsetf
      #:psetf
      #:setq
      #:psetq
      #:simple-let*
      #:simple-let
      #:lexical-let*
      #:lexical-let
      #:let*
      #:let

      ;; variables
      #:var
      #:defvar

      ;; iteration
      #:labeled-for
      #:for
      #:for-in
      #:while
      #:do
      #:do*
      #:dotimes
      #:dolist
      #:doeach

      ;; with
      #:with

      ;; case
      #:switch
      #:case
      #:default

      ;; try throw catch
      #:try

      ;; regex literals
      #:regex

      ;; conditional compilation (IE)
      #:cc-if

      ;; function definition
      #:defun
      #:lambda

      ;; lambda lists
      #:&key
      #:&rest
      #:&body
      #:&optional
      #:&aux
      #:&environment
      #:&key-object

      ;; slot access
      #:with-slots
      #:slot-value

      ;; macros
      #:macrolet
      #:symbol-macrolet
      #:define-symbol-macro
      #:define-script-symbol-macro
      #:defmacro

      ;; lisp eval
      #:lisp

      ;; v v v STUFF WE SHOULD PROBABLY MOVE TO OTHER LIBS v v v

      ;; html generator for javascript
      #:ps-html

      ;; utils
      #:do-set-timeout
      #:min
      #:max
      #:ceiling
      #:abs
      #:sin
      #:cos
      #:tan
      #:acos
      #:asin
      #:atan
      #:exp
      #:log
      #:floor
      #:expt
      #:round
      #:random
      #:oddp
      #:evenp
      #:ignore-errors
      #:concatenate
      #:length
      #:null
      #:@

      ;; js runtime utils
      #:*ps-lisp-library*
      #:mapcar
      #:map-into
      #:map
      #:map-until
      #:member
      #:append
      #:set-difference
      ))
  "All symbols considered part of the Parenscript language.")

(defpackage :parenscript
  (:use :common-lisp)
  (:nicknames :js :ps)
  #.(cons :export *parenscript-lang-exports*)

  ;;; symbols that form the interface to the Parenscript compiler
  (:export
   ;; compiler
   #:compile-script
   #:ps
   #:ps-doc
   #:ps*
   #:ps-inline
   #:ps-inline*

   ;; for parenscript macro definition within lisp
   #:defpsmacro
   #:defmacro/ps
   #:defmacro+ps
   #:import-macros-from-lisp

   ;; gensym
   #:ps-gensym
   #:with-ps-gensyms
   #:*ps-gensym-counter*

   ;; naming and namespaces
   #:ps-package-prefix
   #:obfuscate-package
   #:unobfuscate-package

   ;; printer
   #:*js-string-delimiter*
   #:*js-inline-string-delimiter*
   #:*ps-print-pretty*
   #:*indent-num-spaces*

   ;; deprecated interface
   #:gen-js-name
   #:with-unique-js-names
   #:defjsmacro
   #:js-compile
   #:js-inline
   #:js-inline*
   #:js
   #:js*
   ))

(defpackage :parenscript-special-forms
  (:use))
