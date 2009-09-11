(in-package "CL-USER")

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
      #:== #:!= #:=
      #:=== #:!==
      #:&
      #:^
      #:\|
      #:\&\& #:and
      #:\|\| #:or
      #:>>= #:<<=
      #:*= #:/= #:%= #:+= #:\&= #:^= #:\|= #:~=
      #:incf #:decf
      
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
      #:loop

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
      #:*ps-html-empty-tag-aware-p*
      #:*ps-html-mode*
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
      #:defined
      #:undefined
      #:@
      #:chain
      #:with-lambda
      #:stringp
      #:numberp
      #:functionp
      #:objectp
      #:memoize
      #:append
      #:apply
      #:destructuring-bind

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
      #:map-until
      #:member
      #:append
      #:set-difference
      ))
  (defparameter *parenscript-interface-exports*
    '(;; compiler
      #:*js-target-version*
      #:compile-script
      #:ps
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

  (defparameter *javascript-exports*
    '(;;; for representing js code as s-expressions

      ;; operators
      ; arithmetic
      #:+
      #:-
      #:*
      #:/
      #:%

      ; bitwise
      #:&
      #:|\||
      #:^
      #:~
      #:>>
      #:<<
      #:>>>

      ; assignment
      #:=
      #:+=
      #:-=
      #:*=
      #:/=
      #:%=
      #:&=
      #:\|=
      #:^+
      #:>>=
      #:<<=
      #:>>>=

      ; increment/decrement
      #:++
      #:--

      ; comparison
      #:==
      #:===
      #:!=
      #:!==
      #:>
      #:>=
      #:<
      #:<=

      ; logical
      #:&&
      #:||||
      #:!
      
      ; misc
      #:? ; ternary
      #:|,|
      #:delete
      #:function
      #:get
      #:in
      #:instanceof
      #:new
      #:this
      #:typeof
      #:void
      #:null
      

      ;; statements
      #:block
      #:break
      #:continue
      #:do-while
      #:for
      #:for-in
      #:if
      #:label
      #:return
      #:switch
      #:throw
      #:try
      #:var
      #:while
      #:with

      
      #:unary-operator
      #:literal
      #:array
      #:aref
      #:operator
      #:cond
      #:lambda
      #:object
      #:variable
      #:slot-value
      #:funcall
      #:escape
      ))
  )

(defpackage "PARENSCRIPT"
  (:use "COMMON-LISP" "ANAPHORA")
  (:nicknames "JS" "PS")
  #.(cons :export *parenscript-lang-exports*)
  #.(cons :export *parenscript-interface-exports*)
  #.(cons :export *parenscript-interface-deprecated-exports*)
  #.(cons :export *javascript-exports*)
  )

