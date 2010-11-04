;;;; -*- lisp -*-

(defsystem :parenscript
  :name "parenscript"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :maintainer "Vladimir Sedach <vsedach@gmail.com>"
  :licence "BSD"
  :components
  ((:static-file "parenscript.asd")
   (:module :src
            :serial t
            :components ((:file "package")
                         (:file "utils")
                         (:file "namespace")
                         (:file "parse-lambda-list")
                         (:file "compiler")
                         (:file "printer")
                         (:file "compilation-interface")
                         (:file "special-operators")
                         (:file "macros")
                         (:file "deprecated-interface")
                         (:file "js-dom-symbol-exports")
                         (:module :lib
                                  :components ((:file "ps-html")
                                               (:file "ps-loop")
                                               (:file "ps-dom"))
                                  :depends-on ("compilation-interface"))))
   (:module :runtime
            :components ((:file "ps-runtime-lib"))
            :depends-on (:src)))
  :depends-on (:cl-ppcre :anaphora))
