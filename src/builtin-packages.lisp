(in-package :parenscript)

(defmethod setup-compilation-environment ((comp-env compilation-environment))
  (install-standard-script-packages comp-env)
  (setf (comp-env-current-package comp-env)
	(find-script-package :parenscript-user comp-env))
  comp-env)

(defparameter *javascript-exports*
  (append
   nil
   cl-user::*shared-symbols-ps-js*))

(defparameter *parenscript-exports*
  (append
   *javascript-exports*
   cl-user::*parenscript-lang-exports*
   nil
   ))

(defmethod install-standard-script-packages ((comp-env compilation-environment))
  (list
   (create-script-package
    comp-env
    :name "KEYWORD" :lisp-package :keyword)
   (create-script-package
    comp-env
    :name "GLOBAL" :lisp-package :parenscript.global)
   ;; symbols in the parenscript, javascript and parenscript-user packages are non-prefixed
   (create-script-package
    comp-env
    :name "JAVASCRIPT" :prefix "" :nicknames (list "JS") :lisp-package :parenscript.javascript
    :exports *javascript-exports*
    :secondary-lisp-packages '(:common-lisp))
   (create-script-package
    comp-env
    :name "PARENSCRIPT" :prefix "" :lisp-package :parenscript
    :exports *parenscript-exports*
    :used-packages '(:javascript)
    )
   (create-script-package
    comp-env
    :name "PARENSCRIPT-USER" :prefix "" :lisp-package :parenscript-user
    :secondary-lisp-packages (list :cl-user)
    :used-packages '("PARENSCRIPT")
    :nicknames '("PS-USER" "PAREN-USER"))
   (create-script-package
    comp-env
    :name "PS_GS" :lisp-package :parenscript.ps-gensyms)
   (create-script-package
    comp-env
    :name "UNINTERNED" :prefix "")))