(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *parenscript-lang-exports*
    '(;; literals
      #:t
      #:f
      #:true
      #.(symbol-name 'nil) ; for case-sensitive Lisps like some versions of Allegro
      #:this
      #:false
      #:undefined
      #:{}

      ;; keywords
      #:break
      #:continue

      ;; array literals
      #:array
      #:list
      #:aref
      #:elt
      #:make-array
      #:[]

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
      #:incf #:decf

      ;; body forms
      #:progn

      ;; object literals
      #:create
      #:with-slots

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
      #:flet
      #:labels

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
      #:define-ps-symbol-macro
      #:defmacro

      ;; lisp eval
      #:lisp

      ;; v v v STUFF WE SHOULD PROBABLY MOVE TO OTHER LIBS v v v

      ;; html generator for javascript
      #:*self-closing-tags-p*
      #:ps-html
      #:who-ps-html

      ;; utils
      #:do-set-timeout
      #:max
      #:min
      #:floor
      #:ceiling
      #:round
      #:sin
      #:cos
      #:tan
      #:asin
      #:acos
      #:atan
      #:pi
      #:sinh
      #:cosh
      #:tanh
      #:asinh
      #:acosh
      #:atanh
      #:1+
      #:1-
      #:abs
      #:evenp
      #:oddp
      #:exp
      #:expt
      #:log
      #:sqrt
      #:random
      #:ignore-errors
      #:concatenate
      #:concat-string
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
  (defparameter *parenscript-interface-exports*
    '(;; compiler
      #:compile-script
      #:ps
      #:ps-doc
      #:ps*
      #:ps1*
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
      #:ps-once-only
      #:*ps-gensym-counter*

      ;; naming and namespaces
      #:ps-package-prefix
      #:obfuscate-package
      #:unobfuscate-package

      ;; printer
      #:symbol-to-js-string
      #:*js-string-delimiter*
      #:*js-inline-string-delimiter*
      #:*ps-print-pretty*
      #:*indent-num-spaces*
      ))
  (defparameter *parenscript-interface-deprecated-exports*
    '(;; deprecated interface
      #:define-script-symbol-macro
      #:gen-js-name
      #:with-unique-js-names
      #:defjsmacro
      #:js-compile
      #:js-inline
      #:js-inline*
      #:js
      #:js*
      #:symbol-to-js
      ))
  )

(defpackage :parenscript
  (:use :common-lisp)
  (:nicknames :js :ps)
  #.(cons :export *parenscript-lang-exports*)
  #.(cons :export *parenscript-interface-exports*)
  #.(cons :export *parenscript-interface-deprecated-exports*)
  )

