;;;; -*- lisp -*-

(in-package :cl-user)

(defpackage :parenscript.system
  (:use :cl :asdf))

(in-package :parenscript.system)

(defsystem :parenscript
  :name "parenscript"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :maintainer "Vladimir Sedach <vsedach@gmail.com>"
  :licence "BSD"
  :description "Parenscript is a Lispy language that compiles to JavaScript."
  :components ((:static-file "parenscript.asd")
               (:module :src
                        :serial t
                        :components ((:file "package")
                                     (:file "utils")
                                     (:file "namespace")
                                     (:file "parse-lambda-list")
                                     (:file "compiler")
                                     (:file "special-forms")
                                     (:file "printer")
                                     (:file "compilation-interface")
                                     (:file "deprecated-interface")
                                     (:file "js-dom-symbol-exports")
                                     ;; standard library
                                     (:module :lib
                                              :components ((:file "ps-html")
                                                           (:file "ps-macro-lib"))
                                              :depends-on ("compilation-interface"))))
               (:module :runtime
                        :components ((:file "ps-runtime-lib"))
                        :depends-on (:src)))
  :depends-on ())

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system :parenscript)))) 
  (pushnew :parenscript cl:*features*))

(defmethod asdf:perform ((o test-op) (c (eql (find-system :parenscript))))
  (asdf:operate 'asdf:test-op :parenscript.test))

(defsystem :parenscript.test
  :components ((:module :t
                        :serial t
                        :components ((:file "test-package")
                                     (:file "test")
                                     (:file "ref2test")
                                     (:file "reference-tests")
                                     (:file "ps-tests")
                                     (:file "package-system-tests"))))
  :depends-on (:parenscript :fiveam :cl-ppcre))

(defmethod asdf:perform ((o test-op) (c (eql (find-system :parenscript.test))))
  (asdf:operate 'asdf:load-op :parenscript.test)
  (funcall (intern (symbol-name :run-tests)
                   (find-package :parenscript-test))))
