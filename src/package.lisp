;;; Copyright 2005 Manuel Odendahl
;;; Copyright 2005-2006 Edward Marco Baringer
;;; Copyright 2006 Luca Capello
;;; Copyright 2006 Atilla Lendvai
;;; Copyright 2007-2012 Vladimir Sedach
;;; Copyright 2007 Red Daly
;;; Copyright 2008 Travis Cross

;;; SPDX-License-Identifier: BSD-3-Clause

;;; Redistribution and use in source and binary forms, with or
;;; without modification, are permitted provided that the following
;;; conditions are met:

;;; 1. Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.

;;; 2. Redistributions in binary form must reproduce the above
;;; copyright notice, this list of conditions and the following
;;; disclaimer in the documentation and/or other materials provided
;;; with the distribution.

;;; 3. Neither the name of the copyright holder nor the names of its
;;; contributors may be used to endorse or promote products derived
;;; from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl)

(pushnew :parenscript *features*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :parenscript)
    (named-readtables:defreadtable :parenscript
      (:merge :standard)
      (:case #.(if (eql :upcase (readtable-case *readtable*))
                   :invert
                   (readtable-case *readtable*))))))

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
   #:maybe-once-only
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language

   ;; literals
   #:t
   #:nil

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
   #:mod
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

   ;; if
   #:if
   #:when
   #:unless

   ;; control flow
   #:return
   #:return-from
   #:throw

   ;; assignment and binding
   #:setf
   #:defsetf
   #:psetf
   #:setq
   #:psetq
   #:let*
   #:let

   ;; variables
   #:defvar

   ;; iteration
   #:do
   #:do*
   #:dotimes
   #:dolist
   #:loop

   ;; case
   #:switch
   #:case
   #:default

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

   ;; macros
   #:macrolet
   #:symbol-macrolet
   #:define-symbol-macro
   #:define-ps-symbol-macro
   #:defmacro

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
   #:length
   #:stringp
   #:numberp
   #:functionp
   #:append
   #:apply
   #:destructuring-bind

   ;; js runtime utils
   #:*ps-lisp-library*
   #:mapcar
   #:map-into
   #:map
   #:member
   #:append
   #:set-difference

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-Common Lisp functionality

   ;; DOM accessing utils
   #:inner-html
   #:uri-encode
   #:attribute
   #:offset
   #:scroll
   #:inner
   #:client

   ;; utils
   #:@
   #:chain
   #:defined
   #:undefined
   #:booleanp
   #:objectp
   #:stringify

   ;; html generator for javascript
   #:*ps-html-empty-tag-aware-p*
   #:*ps-html-mode*
   #:ps-html
   #:who-ps-html

   ;; lisp eval
   #:lisp

   ;; js object stuff
   #:delete
   #:typeof
   #:instanceof
   #:new
   #:create

   ;; slot access
   #:with-slots
   #:getprop
   #:in

   ;; literals
   #:regex
   #:this
   #:undefined
   #:{}
   #:false

   ;; iteration
   #:for
   #:for-in
   #:while

   ;; global var
   #:var

   ;; control flow
   #:try
   #:break
   #:continue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deprecated functionality

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
   #:bind
   #:bind*
   ))
