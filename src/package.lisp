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
      #:cc-if)
    "Symbols exported from both the Parenscript and Javascript packages 
that are also valid as Parenscript symbols for the corresponding script packages."))

  

(defpackage parenscript.javascript
  (:use :common-lisp)
  (:nicknames javascript ps-js)
  #.(cons :export *shared-symbols-ps-js*)
  (:export
   ;; translate
   #:js-to-strings
   #:js-to-statement-strings
   )
  (:documentation "The package used to define Javascript special forms.  Most of Parenscript
is defined as macros on top of Javascript special forms"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *parenscript-lang-exports*
    (append 
     *shared-symbols-ps-js*
     '(
       ;; package system
       #:defpackage
       #:in-package

       ;; eval-when
       #:eval-when
       ;; macros
       #:macrolet
       #:symbol-macrolet
       
       ;; lisp eval
       #:lisp
       
       ;; assignment
       #:setf
       
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
       ))
    "List of (uninterned) symbols. Contains all symbols considerred
part of the Parenscript language.  These should be exported within
both the Lisp package and the script package for Parenscript."))

(defpackage :parenscript
  (:use :common-lisp :parenscript.javascript)
  (:nicknames :js :ps)
  #.(cons :export *shared-symbols-ps-js*)
  #.(cons :export *parenscript-lang-exports*)
  (:export
   ;; compiler
   #:compile-script
   #:compile-script-file
   #:compile-script-system
   #:compile-parenscript-file
   #:compile-parenscript-file-to-string
   #:ps-to-string
   #:script
   #:script*
   #:ps
   #:ps*
   #:js
   #:js*
   #:with-new-compilation-environment ; tentative
   #:with-compilation-environment     ; tentative
   #:*compilation-environment*
   
   ;; package system
   #:*enable-package-system*
   #:find-script-package
   #:script-intern
   #:script-export
   #:find-script-symbol
   #:comp-env-current-package
   #:symbol-script-package
   #:script-package-name
   
   ;; for parenscript macro definition within lisp
   #:defscriptmacro
   #:defpsmacro ; should we use one or the other of these?
   #:defmacro/ps
   #:defmacro+ps
   #:import-macros-from-lisp
   
   ;; gensym
   #:with-unique-ps-names
   #:gen-script-name
   #:gen-script-name-string
   #:gen-ps-name

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
   ))

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
   script-equal
   compile-script-form
   ) 
 :parenscript.javascript)

(defpackage parenscript.reader
  (:nicknames parenscript-reader)
  (:use :common-lisp :parenscript)
  (:shadow #:readtablep
           #:readtable-case
           #:copy-readtable
           #:get-macro-character
           #:get-dispatch-macro-character
           #:set-macro-character
           #:set-dispatch-macro-character
           #:make-dispatch-macro-character
           #:set-syntax-from-char
           #:read-preserving-whitespace
           #:read
           #:read-from-string
           #:read-delimited-list
           #:backquote-comma-dot
           #:backquote
           #:backquote-comma
           #:backquote-comma-at
           
           #:*read-eval*
           #:*read-base*
           #:*read-default-float-format*
           #:*read-suppress*
           #:*readtable*
           #:*read-suppress*
           #:*reader-error*
           #:*read-suppress*
           
           #:readtable
           #:backquote
           #:reader-error)
  (:export
    #:read
    #:read-from-string
    #:read-delimited-list)
  (:documentation "The Parenscript reader.  Used for reading Parenscript
forms."))

(defpackage parenscript.global
  (:nicknames "GLOBAL")
  (:documentation "Symbols interned in the global package are serialized in Javascript
as non-prefixed identifiers."))

(defpackage parenscript.user
  (:use :parenscript)
  (:nicknames ps-user parenscript-user)
  (:documentation "The default package a user is inside of when compiling code."))

(defpackage parenscript.asdf
  (:use :parenscript :asdf :common-lisp)
  (:documentation "ASDF extensions that help compile and use Parenscript systems."))