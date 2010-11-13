(in-package #:cl)

(defpackage #:js
  (:shadowing-import-from #:cl
   #:+
   #:-
   #:*
   #:/)
  (:export
   ;; operators
   ;; arithmetic
   #:+
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
   #:in
   #:instanceof
   #:new
   #:typeof
   #:void

   ;; literals
   #:nil
   #:t
   #:f
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
