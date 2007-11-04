(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *parenscript-lang-exports*
    '(
      ;; literals
      #:t
      #:f
      #:true
      #.(symbol-name 'nil) ;; for case-sensitive Lisps like some versions of Allegro
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
      #:defaultf
      #:defsetf
      #:let
      
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
      #:floor
      #:expt
      #:round
      #:random
      #:oddp
      #:evenp
      #:ignore-errors
      #:length
      #:null
      #:@

      ;; libries
      #:*ps-lisp-library*
      #:mapcar
      ))
  "All symbols considerred part of the Parenscript language.")

(defpackage :parenscript
  (:use :common-lisp)
  (:nicknames :js :ps)
  #.(cons :export *parenscript-lang-exports*)

  ;;; symbols that form the interface to the Parenscript compiler
  (:export
   ;; compiler
   #:compile-script
   #:ps
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
