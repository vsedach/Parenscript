(in-package :parenscript)

;;; quote
(defscriptclass script-quote (ps-js::expression)
  ())

;;; Compilation environment stuff

(defvar *compilation-environment* nil
  "The active compilation environment."
;; Right now all code assumes that *compilation-environment* is accurately bound to the
;; current compilation environment--even some functions that take the compilation environment
;; as arguments.
  )

;; environmental considerations
(defgeneric setup-compilation-environment (comp-env)
  (:documentation "Sets up a basic compilation environment prepared for a language user.
This should do things like define packages and set the current package.

Returns the compilation-environment."))

(defun make-basic-compilation-environment ()
  "Creates a compilation environment object from scratch.  Fills it in with the default
script packages (parenscript, global, and parenscript-user)."
  (let ((*compilation-environment* (make-instance 'compilation-environment)))
    (setup-compilation-environment *compilation-environment*)))