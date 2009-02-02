(in-package "COMMON-LISP")

;; These are convenience packages that export JS and browser DOM
;; symbols. If you :use the packages in a package FOO and then
;; obfuscate FOO, it will prevent the JS symbols from getting
;; mangled. As well, a package with symbols for the Prototype library
;; is included.

(defpackage "PS-JS-SYMBOLS"
  (:export
   #:to-fixed
   #:encode-u-r-i-component
   #:size
   #:*array
   #:*date
   #:get-time
   #:arguments
   #:join
   #:prototype
   #:slice
   #:call
   ))

(defpackage "PS-DOM-SYMBOLS"
  (:export
   #:inner-h-t-m-l
   #:document
   #:window
   #:onload
   #:scroll-left
   #:offset-width
   #:offset-height
   #:client-x
   #:client-y
   #:style
   #:top
   #:width
   #:left
   #:display
   #:onmousemove
   #:create-element
   #:set-attribute
   #:append-child
   #:offset-height
   #:offset-width
   #:client-height
   #:client-width
   #:scroll-height
   #:scroll-width
   #:insert-row
   #:insert-cell
   #:value
   #:elements
   #:get-elements-by-class-name
   #:get-element-by-id
   #:onselectstart
   #:set-timeout
   #:set-interval
   ))

(defpackage "PS-PROTOTYPE-LIB-SYMBOLS"
  (:export
   #:*event
   #:observe
   #:*ajax
   #:*request
   #:console
   ))
