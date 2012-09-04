(in-package #:cl)

(pushnew :parenscript *features*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :parenscript)
    (named-readtables:defreadtable :parenscript
      (:merge :standard)
      (:case :invert))))

(named-readtables:in-readtable :parenscript)

(defpackage #:parenscript
  (:use #:cl #:anaphora #:named-readtables)
  (:nicknames #:ps)
  (:export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler interface

   ;; compiler
   #:*js-target-version*
   #:ps
   #:*parenscript-stream*
   #:ps-to-stream
   #:ps-doc
   #:ps-doc*
   #:ps*
   #:ps-inline
   #:ps-inline*
   #:*ps-read-function*
   #:ps-compile-file
   #:ps-compile-stream
   ;; for parenscript macro definition within lisp
   #:defpsmacro
   #:defmacro+ps
   #:import-macros-from-lisp
   #:*defined-operators*
   #:*version*

   ;; gensym
   #:ps-gensym
   #:with-ps-gensyms
   #:ps-once-only
   #:*ps-gensym-counter*

   ;; naming and namespaces
   #:in-package
   #:use-package
   #:ps-package-prefix
   #:obfuscate-package
   #:unobfuscate-package

   ;; printer
   #:symbol-to-js-string
   #:*js-string-delimiter*
   #:*js-inline-string-delimiter*
   #:*ps-print-pretty*
   #:*indent-num-spaces*

   ;; deprecated interface
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
   #:slot-value
   #:compile-script
   #:defmacro/ps
   #:%
   #:==
   #:===
   #:!=
   #:!==
   #:labeled-for
   #:do-set-timeout
   #:concat-string
   #:with
   #:label
   #:f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language

   ;; literals
   #:t
   #:false
   #.(symbol-name 'nil) ; for case-sensitive Lisps like some versions of Allegro
   #:this
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
   ;; logical boolean
   #:not
   #:and
   #:or

   ;; bitwise boolean
   #:logand
   #:logior
   #:logxor
   #:lognot
   #:ash

   #:*
   #:/
   #:rem
   #:+
   #:-
   #:<
   #:>
   #:<=
   #:>=
   #:incf
   #:decf
   #:equal
   #:eql
   #:eq
   #:=

   ;; compile-time stuff
   #:eval-when

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
   #:return-from
   #:throw

   ;; single argument expressions
   #:delete
   #:typeof
   #:instanceof
   #:new

   ;; assignment and binding
   #:setf
   #:defsetf
   #:psetf
   #:setq
   #:psetq
   #:let*
   #:let

   ;; variables
   #:var
   #:defvar

   ;; iteration
   #:for
   #:for-in
   #:while
   #:do
   #:do*
   #:dotimes
   #:dolist
   #:loop

   ;; case
   #:switch
   #:case
   #:default

   ;; try throw catch
   #:try

   ;; regex literals
   #:regex

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
   #:getprop
   #:in

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
   #:*ps-html-empty-tag-aware-p*
   #:*ps-html-mode*
   #:ps-html
   #:who-ps-html

   ;; utils
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
   #:stringify
   #:length
   #:defined
   #:undefined
   #:@
   #:chain
   #:stringp
   #:numberp
   #:functionp
   #:booleanp
   #:objectp
   #:append
   #:apply
   #:destructuring-bind

   ;; binding
   #:bind
   #:bind*

   ;; DOM accessing utils
   #:inner-html
   #:uri-encode
   #:attribute
   #:offset
   #:scroll
   #:inner
   #:client

   ;; js runtime utils
   #:*ps-lisp-library*
   #:mapcar
   #:map-into
   #:map
   #:member
   #:append
   #:set-difference
   ))
