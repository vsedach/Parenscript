;;; Copyright 2005 Manuel Odendahl
;;; Copyright 2005-2006 Edward Marco Baringer
;;; Copyright 2006 Luca Capello
;;; Copyright 2010-2012 Vladimir Sedach
;;; Copyright 2012, 2014, 2015 Boris Smilga

;;; SPDX-License-Identifier: BSD-3-Clause

;;; Redistribution and use in source and binary forms, with or
;;; without modification, are permitted provided that the following
;;; conditions are met:

;;; 1. Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.

;;; 2. Redistributions in binary form must reproduce the above
;;; copyright notice, this list of conditions and the following
;;; disclaimer in the documentation and/or other materials provided
;;; with the distribution.

;;; 3. Neither the name of the copyright holder nor the names of its
;;; contributors may be used to endorse or promote products derived
;;; from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package #:parenscript)
(in-readtable :parenscript)

;;; PS operators and macros that aren't present in the Common Lisp
;;; standard but exported by Parenscript, and their Common Lisp
;;; equivalent definitions

(defmacro define-trivial-special-ops (&rest mappings)
  `(progn ,@(loop for (form-name js-primitive) on mappings by #'cddr collect
                 `(define-expression-operator ,form-name (&rest args)
                    (cons ',js-primitive (mapcar #'compile-expression args))))))

(define-trivial-special-ops
  array      ps-js:array
  instanceof ps-js:instanceof
  typeof     ps-js:typeof
  new        ps-js:new
  delete     ps-js:delete
  in         ps-js:in ;; maybe rename to slot-boundp?
  break      ps-js:break
  <<         ps-js:<<
  >>         ps-js:>>
  )

(define-statement-operator continue (&optional label)
  `(ps-js:continue ,label))

(define-statement-operator switch (test-expr &rest clauses)
  `(ps-js:switch ,(compile-expression test-expr)
     ,@(let ((in-case? t))
         (loop for (val . body) in clauses collect
              (cons (if (eq val 'default)
                        'ps-js:default
                        (compile-expression val))
                    (flatten-blocks
                     (mapcar #'compile-statement body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; objects

(define-expression-operator create (&rest arrows)
  (let ((allow-accessors (js-target-at-least "1.8.5")))
    (cons
     'ps-js:object
     (loop for (key val-expr) on arrows by #'cddr
           for (accessor . accessor-args) =
             (when (and allow-accessors
                        (consp key)
                        (symbolp (first  key))
                        (symbolp (second key)))
               (case (first key)
                 (get (and (null (third key))
                           `((ps-js:get ,(second key)))))
                 (set (and (symbolp (third key)) (null (fourth key))
                           `((ps-js:set ,(second key)) ,(third key))))))
           collecting
             (if accessor
                 (list accessor accessor-args
                       (let ((*function-block-names* ()))
                         (compile-function-body (third accessor)
                                                (list val-expr))))
                 (cons (cond ((and (symbolp key) (reserved-symbol-p key))
                              (reserved-symbol-p key))
                             ((or (stringp key) (numberp key) (symbolp key))
                              key)
                             ((and (consp key)
                                   (eq 'quote (first  key))
                                   (symbolp   (second key))
                                   (null      (third  key)))
                              (symbol-to-js-string (second key)))
                             (t
                              (error "Slot key ~s is not one of ~
                                      ~{~a~#[~;, or ~:;, ~]~}."
                                     key
                                     (list* "symbol" "string" "number"
                                            (when allow-accessors
                                              '("accessor spec"))))))
                       (compile-expression val-expr)))))))

(define-expression-operator %js-getprop (obj slot)
  (let ((expanded-slot (ps-macroexpand slot))
        (obj (compile-expression obj)))
    (if (and (listp expanded-slot)
             (eq 'quote (car expanded-slot)))
        (aif (or (reserved-symbol-p (second expanded-slot))
                 (and (keywordp (second expanded-slot)) (second expanded-slot)))
             `(ps-js:aref ,obj ,it)
             `(ps-js:getprop ,obj ,(second expanded-slot)))
        `(ps-js:aref ,obj ,(compile-expression slot)))))

(defpsmacro getprop (obj &rest slots)
  (if (null (rest slots))
      `(%js-getprop ,obj ,(first slots))
      `(getprop (getprop ,obj ,(first slots)) ,@(rest slots))))

(defpsmacro @ (obj &rest props)
  "Handy getprop/aref composition macro."
  (if props
      `(@ (getprop ,obj ,(if (symbolp (car props))
                             `',(car props)
                             (car props)))
          ,@(cdr props))
      obj))

(defun chain (method-calls)
  (let ((chain (car method-calls)))
    (dolist (next (cdr method-calls))
      (setf chain (if (consp next)
                      `(funcall (@ ,chain ,(car next)) ,@(cdr next))
                      `(@ ,chain ,next))))
    chain))

(defpsmacro chain (&rest method-calls)
  (chain method-calls))

;;; var

(define-expression-operator var (name &optional (value (values) value?) docstr)
  (declare (ignore docstr))
  (push name *vars-needing-to-be-declared*)
  (when value? (compile-expression `(setf ,name ,value))))

(define-statement-operator var (name &optional (value (values) value?) docstr)
  (let ((value (ps-macroexpand value)))
    (if (and (listp value) (eq 'progn (car value)))
        (ps-compile `(progn ,@(butlast (cdr value))
                            (var ,name ,(car (last value)))))
        `(ps-js:var ,(ps-macroexpand name)
                    ,@(when value? (list (compile-expression value) docstr))))))

(defmacro var (name &optional value docstr)
  `(defparameter ,name ,value ,@(when docstr (list docstr))))

;;; iteration

(define-statement-operator for (init-forms cond-forms step-forms &body body)
  (let ((init-forms (make-for-vars/inits init-forms)))
    `(ps-js:for ,init-forms
                ,(mapcar #'compile-expression cond-forms)
                ,(mapcar #'compile-expression step-forms)
                ,(compile-loop-body (mapcar #'car init-forms) body))))

(define-statement-operator for-in ((var object) &rest body)
  `(ps-js:for-in ,(compile-expression var)
                 ,(compile-expression object)
                 ,(compile-loop-body (list var) body)))

;;; misc

(define-statement-operator try (form &rest clauses)
  (let ((catch   (cdr (assoc :catch clauses)))
        (finally (cdr (assoc :finally clauses))))
    (assert (not (cdar catch)) ()
            "Sorry, currently only simple catch forms are supported.")
    (assert (or catch finally) ()
            "Try form should have either a catch or a finally clause or both.")
    `(ps-js:try
      ,(compile-statement `(progn ,form))
      :catch ,(when catch
                    (list (caar catch)
                          (compile-statement `(progn ,@(cdr catch)))))
      :finally ,(when finally
                      (compile-statement `(progn ,@finally))))))

(define-expression-operator regex (regex)
  `(ps-js:regex ,(string regex)))

(define-expression-operator lisp (lisp-form)
  ;; (ps (foo (lisp bar))) is like (ps* `(foo ,bar))
  ;; When called from inside of ps*, lisp-form has access to the
  ;; dynamic environment only, analogous to eval.
  `(ps-js:escape
    (with-output-to-string (*psw-stream*)
      (let ((compile-expression?   ,compile-expression?)
	    (*js-string-delimiter* ,*js-string-delimiter*)
            (eval-results          (multiple-value-list ,lisp-form)))
        (when eval-results
          (parenscript-print (ps-compile (car eval-results)) t))))))

(defun lisp (x) x)

(defpsmacro undefined (x)
  `(eql "undefined" (typeof ,x)))

(defpsmacro defined (x)
  `(not (undefined ,x)))

(defpsmacro objectp (x)
  `(string= (typeof ,x) "object"))

(define-ps-symbol-macro {} (create))

(defpsmacro [] (&rest args)
  `(array ,@(mapcar (lambda (arg)
                      (if (and (consp arg) (not (equal '[] (car arg))))
                          (cons '[] arg)
                          arg))
                    args)))

(defpsmacro stringify (&rest things)
  (if (and (= (length things) 1) (stringp (car things)))
      (car things)
      `(funcall (getprop (list ,@things) 'join) "")))
(defun stringify (&rest things)
  "Like concatenate but prints all of its arguments."
  (format nil "~{~A~}" things))

(define-ps-symbol-macro false ps-js:false)
(defvar false nil)
