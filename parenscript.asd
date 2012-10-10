;;;; -*- lisp -*-

(defsystem :parenscript
  :name "parenscript"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :maintainer "Vladimir Sedach <vsedach@gmail.com>"
  :licence "BSD"
  :description "Lisp to JavaScript transpiler"
  :components
  ((:static-file "parenscript.asd")
   (:module :src
            :serial t
            :components ((:file "package")
                         (:file "js-dom-symbol-exports") ;; has to be loaded here, ps-js-symbols externals are re-exported from #:parenscript package
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
                         (:module :lib
                                  :components ((:file "ps-html")
                                               (:file "ps-loop")
                                               (:file "ps-dom"))
                                  :depends-on ("compilation-interface"))))
   (:module :runtime
            :components ((:file "ps-runtime-lib"))
            :depends-on (:src)))
  :depends-on (:cl-ppcre :anaphora :named-readtables))
