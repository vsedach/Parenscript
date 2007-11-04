;;;; -*- lisp -*-

(in-package :cl-user)

(defpackage :parenscript.system
  (:use :cl :asdf))

(in-package :parenscript.system)

(defsystem :parenscript
  :name "parenscript"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :version "0"
  :maintainer "Vladimir Sedach <vsedach@gmail.com>"
  :licence "BSD"
  :description "Parenscript is a lispy language that compiles to Javascript."
  :components ((:static-file "parenscript.asd")
               (:module :src
                :components ((:file "package")
                             (:file "utils" :depends-on ("package"))
                             (:file "namespace" :depends-on ("package"))
                             (:file "parse-lambda-list" :depends-on ("package"))
			     (:file "compiler" :depends-on ("namespace"))
			     (:file "js-macrology" :depends-on ("compiler"))
			     (:file "ps-macrology" :depends-on ("utils" "js-macrology" "parse-lambda-list"))
			     (:file "printer" :depends-on ("ps-macrology"))
			     (:file "compilation-interface" :depends-on ("package" "printer"))
                             (:file "deprecated-interface" :depends-on ("compilation-interface"))
			     ;; standard library
                             (:module :lib
                                      :components ((:static-file "functional.lisp")
						   (:file "js-html")
						   (:file "css"    )
						   (:file "js-utils")
                                                   (:file "js-lib"))
				      :depends-on ("compilation-interface")))))
  :depends-on ())

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
                             (:file "reference-tests" :depends-on ("test"))
                             (:file "ps-tests" :depends-on ("test"))
			     (:file "package-system-tests" :depends-on ("test"))))))
			     

(defmethod asdf:perform ((o test-op) (c (eql (find-system :parenscript.test))))
  (asdf:operate 'asdf:load-op :parenscript.test)
  (funcall (intern (symbol-name :run-tests)
                   (find-package :parenscript-test))))
