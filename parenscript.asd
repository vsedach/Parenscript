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
  :components ((:static-file "parenscript.asd")
               (:module :src
                :components ((:file "package")
                             (:file "utils" :depends-on ("package"))
                             (:file "defgenerics" :depends-on ("package"))
                             (:file "js" :depends-on ("package" "utils" "defgenerics"))
                             (:file "js-html" :depends-on ("package" "js" "utils"))
                             (:file "css" :depends-on ("package" "utils"))
                             (:file "compile-js" :depends-on ("package" "js"))
                             (:module :lib
                                      :components ((:static-file "functional.lisp")))))))

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system :parenscript)))) 
  (pushnew :parenscript cl:*features*))

(defmethod asdf:perform ((o test-op) (c (eql (find-system :parenscript))))
  (asdf:operate 'asdf:test-op :parenscript.test))

(defsystem :parenscript.test
  :depends-on (:parenscript :fiveam :cl-ppcre)
  :components ((:module :t
                :components ((:file "test-package")
                             (:file "test" :depends-on ("test-package"))
                             (:file "ref2test" :depends-on ("test"))
                             (:file "reference-tests" :depends-on ("test"))))))

(defmethod asdf:perform ((o test-op) (c (eql (find-system :parenscript.test))))
  (asdf:operate 'asdf:load-op :parenscript.test)
  (funcall (intern (symbol-name :run-tests)
                   (find-package :js-test))))
