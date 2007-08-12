(in-package :cl-user)
;;;; Package definitions for the Parenscript 
;; #: 

(eval-when (:compile-toplevel :load-toplevel)
  ;; exports shared between PARENSCRIPT and PARENSCRIPT.JAVASCRIPT
  (defparameter *shared-symbols-ps-js*
    '(
      ;; literals
      #:t
      #:f
      #:true
      "NIL"
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
      
      ;; assignment
      #:setf
      
      ;; variables
      #:defvar
      
      ;; iteration
      #:for
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
      #:cc-if)
    "Symbols exported from both the Parenscript and Javascript packages 
that are also valid as Parenscript symbols for the corresponding script packages."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *parenscript-lang-exports*
    (append 
     *shared-symbols-ps-js*
     '(
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
       #:optional-args

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
       
       ;; assignment
       #:setf
       #:defaultf

       #:let
       
       ;; iteration
       #:do
       #:dotimes
       #:dolist
       #:doeach
       #:while
       
       ;; v v v STUFF WE SHOULD PROBABLY MOVE TO OTHER LIBS v v v
       
       ;; CSS
       #:css
       #:css-to-string
       #:css-inline
       #:css-file

       ;; html generator for javascript
       #:html

       ;; utils
       #:do-set-timeout
       ))
    "List of (uninterned) symbols. Contains all symbols considerred
part of the Parenscript language.  These should be exported within
both the Lisp package and the script package for Parenscript."))

(defpackage :parenscript
  (:use :common-lisp)
  (:nicknames :js :ps)
  #.(cons :export *shared-symbols-ps-js*)
  #.(cons :export *parenscript-lang-exports*)
  (:export
   ;; compiler
   #:compile-script
   #:ps
   #:ps*
   
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
   #:*obfuscate-identifiers*
   #:*package-prefix-style*
   #:ps-package-prefix

   ;; deprecated interface
   #:gen-js-name
   #:gen-js-name-string
   #:with-unique-js-names
   #:defjsmacro
   #:js-compile
   #:js-inline
   #:js-inline*
   #:js-file
   #:js-script
   #:js-to-statement-strings
   #:js
   #:js*
   ))

