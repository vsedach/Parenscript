(in-package :cl-user)

(defpackage :parenscript
  (:use :common-lisp)
  (:nicknames :js :ps)
  (:export
   ;; addition js symbols
   #:new

   ;; literals
   #:t
   #:nil
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
   #:++ #:--
   #:1+ #:1-
   #:incf #:decf

   ;; body forms
   #:progn

   ;; function definition
   #:defun
   #:lambda

   ;; object literals
   #:create
   #:slot-value
   #:with-slots

   ;; macros
   #:macrolet
   #:symbol-macrolet

   ;; lisp eval
   #:lisp

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

   ;; assignment
   #:setf

   ;; variables
   #:defvar
   #:let

   ;; iteration
   #:do
   #:dotimes
   #:dolist
   #:doeach
   #:while

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

   ;; math library
   #:floor
   #:random

   ;; html generator for javascript
   #:html

   ;; compiler
   #:compile-script
   #:script
   #:with-new-compilation-environment ; tentative
   #:with-compilation-environment     ; tentative
   
   ;; for parenscript macro definition within lisp
   #:defscriptmacro #:defpsmacro ; should we use one or the other of these?
   #:defmacro/js
   #:defmacro+js
   #:import-macros-from-lisp
   
   ;; util
   #:with-unique-js-names
   #:gen-js-name
   #:gen-js-name-string

   ;; CSS
   #:css
   #:css-to-string
   #:css-inline
   #:css-file

   #:compile-parenscript-file
   #:compile-parenscript-file-to-string

   ;; deprecated interface
   #:defjsmacro
   #:js-compile
   #:js ; replaced by #:script
   #:js*
   #:js-inline
   #:js-inline*
   #:js-file
   #:js-script
   #:js-to-strings
   #:js-to-statement-strings
   #:js-to-string
   #:js-to-line
   ))
