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
                         (:file "js-ir-package")
                         (:file "utils")
                         (:file "namespace")
                         (:file "compiler")
                         (:file "printer")
                         (:file "compilation-interface")
                         (:file "non-cl")
                         (:file "special-operators")
                         (:file "parse-lambda-list")
                         (:file "function-definition")
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
  :depends-on (:cl-ppcre :anaphora :named-readtables))
