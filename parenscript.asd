;;;; -*- lisp -*-

(in-package :cl-user)

(defpackage :parenscript.system
  (:use :cl :asdf))

(in-package :parenscript.system)

(defsystem :parenscript
  :name "parenscript"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :version "0"
  :maintainer "Edward Marco Baringer <mb@bese.it>"
  :licence "BSD"
  :description "js - javascript compiler"

  :perform (load-op :after (op araneida)
                    (pushnew :parenscript cl:*features*))
  :components ((:static-file "parenscript.asd")
               (:module :src
                :components ((:file "package")
                             (:file "utils" :depends-on ("package"))
                             (:file "defgenerics" :depends-on ("package"))
                             (:file "js" :depends-on ("package" "utils" "defgenerics"))
                             (:file "js-html" :depends-on ("package" "js" "utils"))
                             (:file "css" :depends-on ("package" "utils"))))))

(defsystem :parenscript.test
  :depends-on (:parenscript :fiveam :cl-ppcre)
  :components ((:module :t
                :components ((:file "test-package")
                             (:file "test" :depends-on ("test-package"))
                             (:file "ref2test" :depends-on ("test"))
                             (:file "reference-tests" :depends-on ("test"))))))
