(in-package :parenscript.asdf)

(defvar *parenscript-file-extension* "paren")

;;; ASDF manual: http://constantly.at/lisp/asdf/index.html

;;; a parenscript file is a source file:
;;; A source file is any file that the system does not know how to generate
;;; from other components of the system.
(defclass asdf::parenscript-file (asdf:source-file)
  ())

(defclass asdf::parenscript-compile-op (asdf:operation)
  ((output-spec :initarg :output-spec :initform :javascript :accessor output-spec)
   (comp-env :initarg :comp-env :initform nil :accessor comp-env)
   (pretty-print-p :initarg :pretty-print :initform nil :accessor pretty-print-p)
   (output-stream :initarg :output-stream :initform *standard-output* :accessor output-stream)
   (force-p :initarg :force-p :initform nil :accessor force-p
	    :documentation "T to force compilation."))
  (:documentation "The operation used in conjunction with parenscript:compile-script-system."))

;;;; STANDARD LISP COMPILATION

;;; file extension for parenscript files is ".paren"
;;; e.g. (defmethod source-file-type ((c cl-source-file) (s module)) "lisp")
(defmethod asdf:source-file-type ((c asdf::parenscript-file) (s asdf:module))
  (declare (ignore c) (ignore s))
  *parenscript-file-extension*)

;;; when you compile the system, compile the Parenscript files in it.
(defmethod asdf:perform ((op compile-op) (paren-file asdf::parenscript-file))
;  (parenscript:compile-parenscript-file (component-pathname paren-file)))
  )

;;; when you load the system, do nothing with the parenscript files.  This could
;;; be enhanced so that files are automatically installed into the appropriate web
;;; framework, etc.  for now we do nothing.
(defmethod asdf:perform ((op load-op) (paren-file asdf::parenscript-file))
  nil)

;;;; OUR CUSTOM PARENSCRIPT COMPILATION

(defmethod output-files ((op asdf::parenscript-compile-op) general-component)
  nil)

(defmethod perform ((op asdf::parenscript-compile-op) general-component)
;  (format t "General component: ~A~%" general-component)
  (call-next-method))

(defmethod perform ((op asdf::parenscript-compile-op) (file asdf:source-file))
;  (format t "Source file ignored: ~A~%" file)
  nil)

(defmethod perform ((op asdf::parenscript-compile-op) (file asdf::parenscript-file))
  (compile-script-file (component-pathname file)
	 :comp-env (comp-env op)
	 :output-spec (output-spec op)
	 :pretty-print (pretty-print-p op)
	 :output-stream (output-stream op))
  (write-char #\Newline (output-stream op)))

(defmethod operation-done-p ((op asdf::parenscript-compile-op) general-component)
  (call-next-method)
  nil)

(defmethod operation-done-p ((op asdf::parenscript-compile-op) (file asdf::parenscript-file))
  (and (not (force-p op))
       (call-next-method)))


;;; FIXME: we simply copy load-op's dependencies.  this is Just Not Right.
(defmethod asdf:component-depends-on ((op asdf::parenscript-compile-op) (c component))
  (let ((what-would-load-op-do (cdr (assoc 'load-op
                                           (slot-value c 'asdf::in-order-to)))))
    (mapcar (lambda (dep)
              (if (eq (car dep) 'load-op)
                  (cons 'asdf::parenscript-compile-op (cdr dep))
                  dep))
            what-would-load-op-do)))