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
    :components ((:file "package")
		 (:file "utils" :depends-on ("package"))
		 (:file "defgenerics" :depends-on ("package"))
		 (:file "js" :depends-on ("package" "utils" "defgenerics"))
		 (:file "js-html" :depends-on ("package" "js" "utils"))
		 (:file "css" :depends-on ("package" "utils"))))
