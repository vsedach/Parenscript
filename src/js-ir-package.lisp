(in-package #:parenscript)
(in-readtable :parenscript)

(defpackage #:ps-js
  (:use)
  (:export
   ;; operators
   ;; arithmetic
   #:+
   #:unary-plus
   #:-
   #:negate
   #:*
   #:/
   #:%

   ;; bitwise
   #:&
   #:\|
   #:^
   #:~
   #:>>
   #:<<
   #:>>>

   ;; assignment
   #:=
   #:+=
   #:-=
   #:*=
   #:/=
   #:%=
   #:&=
   #:\|=
   #:^=
   #:~=
   #:>>=
   #:<<=
   #:>>>=

   ;; increment/decrement
   #:++
   #:--
   #:post++
   #:post--

   ;; comparison
   #:==
   #:===
   #:!=
   #:!==
   #:>
   #:>=
   #:<
   #:<=

   ;; logical
   #:&&
   #:\|\|
   #:!

   ;; misc
   #:? ;; ternary
   #:|,|
   #:delete
   #:function
   #:get
   #:set
   #:in
   #:instanceof
   #:new
   #:typeof
   #:void

   ;; literals
   #:nil
   #:t
   #:false
   #:undefined
   #:this

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
   #:default
   #:throw
   #:try
   #:var
   #:while
   #:with

   #:array
   #:aref
   #:cond
   #:lambda
   #:defun
   #:object
   #:getprop
   #:funcall
   #:escape
   #:regex
   ))
