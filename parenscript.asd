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
			     (:file "js-source-model" :depends-on ("package" "utils"))
			     (:file "ps-source-model" :depends-on ("js-source-model"))
			     (:file "parser" :depends-on ("js-source-model" "ps-source-model"))
			     (:file "builtin-packages" :depends-on ("parser"))
			     (:file "deprecated-interface" :depends-on ("parser"))
			     (:file "js-macrology" :depends-on ("deprecated-interface"))
			     (:file "ps-macrology" :depends-on ("js-macrology"))
			     (:file "js-translation" :depends-on ("ps-macrology"))
;			     (:file "js-ugly-translation" :depends-on ("js-translation"))
			     (:file "reader" :depends-on ("parser"))
			     (:file "compilation-interface" :depends-on ("package" "reader" "js-translation" "builtin-packages")); "js-ugly-translation"))
			     (:file "paren-asdf" :depends-on ("package" "compilation-interface"))
			     ;; standard library
                             (:module :lib
                                      :components ((:static-file "functional.lisp")
						   (:file "js-html")
						   (:file "css"    )
						   (:file "js-utils"))
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
