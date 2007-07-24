(in-package :cl-user)

(defpackage parenscript.javascript
  (:use :common-lisp)
  (:nicknames javascript ps-js)
  (:export

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
   #:cc-if
   
   ;; translate
   #:js-to-strings
   #:js-to-statement-strings
   )
  (:documentation "The package used to define Javascript special forms.  Most of Parenscript
is defined as macros on top of Javascript special forms"))

(defpackage :parenscript
  (:use :common-lisp :parenscript.javascript)
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
   #:compile-parenscript-file
   #:compile-parenscript-file-to-string
   #:script
   #:with-new-compilation-environment ; tentative
   #:with-compilation-environment     ; tentative
   #:*compilation-environment*
   
   ;; package system
   #:find-script-package
   #:script-intern
   #:script-export
   #:find-script-symbol
   #:comp-env-current-package
   #:symbol-script-package
   #:script-package-name
   
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
   )
  (:intern 
   #:define-script-special-form
   #:defscriptclass
   #:symbol-to-js
   #:script-quote
   #:*package-prefix-style*
   #:*script-macro-env*
   #:compile-to-statement
   #:compile-to-block
   #:compile-to-symbol
   #:compile-to-expression
   #:list-join
   #:list-to-string
   #:append-to-last
   #:prepend-to-first
   #:string-join
   #:val-to-string
   #:string-split
   #:script-special-form-p
   #:make-macro-env-dictionary
   #:compile-script-form
   )
  )

(in-package :parenscript)

(import 
 '(defscriptclass
   define-script-special-form
   defscriptmacro
   symbol-to-js
   script-quote
   *package-prefix-style*
   *script-macro-env*
   compile-to-statement
   compile-to-block
   compile-to-symbol
   compile-to-expression
   symbol-script-package
   script-package-name
   list-join
   list-to-string
   append-to-last
   prepend-to-first
   string-join
   val-to-string
   string-split
   script-special-form-p
   make-macro-env-dictionary
   js-equal
   compile-script-form
   ) 
 :parenscript.javascript)

(defpackage parenscript.reader
  (:nicknames parenscript-reader)
  (:use :common-lisp :parenscript)
  (:shadow readtablep
           readtable-case
           copy-readtable
           get-macro-character
           get-dispatch-macro-character
           set-macro-character
           set-dispatch-macro-character
           make-dispatch-macro-character
           set-syntax-from-char
           read-preserving-whitespace
           read
           read-from-string
           read-delimited-list
           backquote-comma-dot
           backquote
           backquote-comma
           backquote-comma-at
           
           *read-eval*
           *read-base*
           *read-default-float-format*
           *read-suppress*
           *readtable*
           *read-suppress*
           *reader-error*
           *read-suppress*
           
           readtable
           backquote
           reader-error)
  (:export
    read
    read-from-string
    read-delimited-list))

(defpackage parenscript.global
  (:nicknames global)
  (:documentation "Symbols interned in the global package are serialized in Javascript
as non-prefixed identifiers."))

(defpackage parenscript.user
  (:nicknames ps-user paren-user parenscript-user)
  (:documentation "The default package a user is inside of when compiling code."))