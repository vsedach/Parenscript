;; Tracing macro courtesy of William Halliburton
;; <whalliburton@gmail.com>, logs to Firebug console

;; On a happier note here is a macro I wrote to enable
;; tracing-ala-cl. Works with firebug. You'll need to (defvar
;; *trace-level*). I don't do indentation but that would be an easy
;; addition.

(defpsmacro console (&rest rest)
  `(console.log ,@rest))

(defpsmacro defun-trace (name args &rest body)
  (let* ((sname (ps::symbol-to-js name))
         (tname (ps-gensym name))
         (arg-names (loop for arg in args
                       unless (eq arg '&optional)
                       collect (if (consp arg) (car arg) arg)))
         (argpairs
          (loop for arg in arg-names
             nconc (list (ps::symbol-to-js arg) arg))))
    `(progn
       (defun ,name ,arg-names
         (console *trace-level* ,sname ":" ,@argpairs)
         (incf *trace-level*)
         (let* ((rtn (,tname ,@arg-names)))
           (decf *trace-level*)
           (console *trace-level* ,sname "returned" rtn)
           (return rtn)))
       (defun ,tname ,args
         ,@body))))