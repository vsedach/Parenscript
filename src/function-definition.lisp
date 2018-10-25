;;; Copyright 2011 Vladimir Sedach
;;; Copyright 2014-2015 Boris Smilga
;;; Copyright 2014 Max Rottenkolber

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lambda lists

(defun parse-key-spec (key-spec)
  "parses an &key parameter.  Returns 5 values:
var, init-form,  keyword-name, supplied-p-var, init-form-supplied-p.

Syntax of key spec:
[&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}*
"
  (let* ((var (cond ((symbolp key-spec) key-spec)
                    ((and (listp key-spec) (symbolp (first key-spec))) (first key-spec))
                    ((and (listp key-spec) (listp (first key-spec)))   (second (first key-spec)))))
         (keyword-name (if (and (listp key-spec) (listp (first key-spec)))
                           (first (first key-spec))
                           (intern (string var) :keyword)))
         (init-form (if (listp key-spec) (second key-spec) nil))
         (init-form-supplied-p (if (listp key-spec) t nil))
         (supplied-p-var (if (listp key-spec) (third key-spec) nil)))
    (values var init-form keyword-name supplied-p-var init-form-supplied-p)))

(defun parse-optional-spec (spec)
  "Parses an &optional parameter.  Returns 3 values: var, init-form, supplied-p-var.
[&optional {var | (var [init-form [supplied-p-parameter]])}*] "
  (let* ((var (cond ((symbolp spec) spec)
                    ((and (listp spec) (first spec)))))
         (init-form (if (listp spec) (second spec)))
         (supplied-p-var (if (listp spec) (third spec))))
    (values var init-form supplied-p-var)))

(defun parse-body (body &key allow-docstring)
  "Parses a function or block body, which may or may not include a
docstring.  Returns 2 or 3 values: a docstring (if allowed for), a list
of (declare ...) forms, and the remaining body."
  (let (docstring declarations)
    (loop while
         (cond ((and (consp (car body)) (eq (caar body) 'declare))
                (push (pop body) declarations))
               ((and allow-docstring (not docstring)
                     (stringp (car body)) (cdr body))
                (setf docstring (pop body)))))
    (values body declarations docstring)))

(defun parse-extended-function (lambda-list body)
  "The lambda list is transformed as follows:

* standard and optional variables are the mapped directly into
  the js-lambda list

* keyword variables are not included in the js-lambda list, but
  instead are obtained from the magic js ARGUMENTS
  pseudo-array. Code assigning values to keyword vars is
  prepended to the body of the function."
  (multiple-value-bind (requireds optionals rest? rest keys? keys allow? aux?
                                  aux more? more-context more-count key-object)
      (parse-lambda-list lambda-list)
    (declare (ignore allow? aux? aux more? more-context more-count key-object))
    (let* ( ;; optionals are of form (var default-value)
           (effective-args
            (remove-if #'null
                       (append requireds
                               (mapcar #'parse-optional-spec optionals))))
           (opt-forms
            (mapcar (lambda (opt-spec)
                      (multiple-value-bind (name value suppl)
                          (parse-optional-spec opt-spec)
                        (cond (suppl
                               `(progn
                                  (var ,suppl (not (eql ,name undefined)))
                                  ,@(when value
                                          `((when (not ,suppl) (setf ,name ,value))))))
                              (value
                               `(when (eql ,name undefined)
                                  (setf ,name ,value))))))
                    optionals))
           (key-forms
            (when keys?
              (with-ps-gensyms (n)
                (let (defaults assigns)
                  (mapc
                   (lambda (k)
                     (multiple-value-bind (var init-form keyword-str suppl)
                         (parse-key-spec k)
                       (push `(var ,var ,@(when init-form `((if (undefined ,var) ,init-form ,var)))) defaults)
                       (when suppl (push `(var ,suppl) defaults))
                       (push `(,keyword-str
                               (setf ,var (aref arguments (1+ ,n))
                                     ,@(when suppl `(,suppl t))))
                             assigns)))
                   (reverse keys))
                  `((loop for ,n from ,(length requireds) below (length arguments) by 2 do
                          (case (aref arguments ,n)
                            ,@assigns))
                    ,@defaults)))))
           (rest-form
            (when rest?
              `(var ,rest
                    ((@ Array prototype slice call)
                     arguments ,(length effective-args))))))
      (multiple-value-bind (fun-body declarations docstring)
          (parse-body body :allow-docstring t)
        (values effective-args
                (append declarations
                        opt-forms key-forms (awhen rest-form (list it))
                        fun-body)
                docstring)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common

(defun collapse-function-return-blocks (body)
  (append (butlast body)
          (let ((last (ps-macroexpand (car (last body)))))
            (if (and (listp last) (eq 'block (car last))) ; no need for a block at the end of a function body
                (progn (push (or (second last) 'nilBlock) *function-block-names*)
                       (cddr last))
                (list last)))))

(defun compile-function-body (args body)
  (with-declaration-effects (body body)
    (let* ((in-function-scope?            t)
           (*vars-needing-to-be-declared* ())
           (*used-up-names*               ())
           (*enclosing-function-arguments*
            (append args *enclosing-function-arguments*))
           (*enclosing-lexicals*
            (set-difference *enclosing-lexicals* args))
           (collapsed-body
            (collapse-function-return-blocks body))
           (suppress-values?
            ;; probably buggy; this should be done after macroexpansion
            (tree-find 'values collapsed-body))
           (*dynamic-return-tags*
            (append (mapcar (lambda (x) (cons x nil))
                            *function-block-names*)
                    *dynamic-return-tags*))
           (body
            (let ((in-loop-scope?                 nil)
                  (*loop-scope-lexicals*           ())
                  (*loop-scope-lexicals-captured*  ()))
              (cdr
               (wrap-for-dynamic-return
                *function-block-names*
                (compile-statement
                 `(return-from %function (progn ,@collapsed-body)))))))
           (var-decls
            (compile-statement
             `(progn
                ,@(mapcar
                   (lambda (var) `(var ,var))
                   (remove-duplicates *vars-needing-to-be-declared*))))))
      (when in-loop-scope?
        (setf *loop-scope-lexicals-captured*
              (append (intersection (flatten body) *loop-scope-lexicals*)
                      *loop-scope-lexicals-captured*)))
      `(ps-js:block ,@(reverse (cdr var-decls))
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lambda

(define-expression-operator lambda (lambda-list &rest body)
  (multiple-value-bind (effective-args effective-body)
      (parse-extended-function lambda-list body)
    `(ps-js:lambda ,effective-args
       ,(let ((*function-block-names* ()))
          (compile-function-body effective-args effective-body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; named functions

(defun compile-named-function-body (name lambda-list body)
  (let ((*enclosing-lexicals* (cons name *enclosing-lexicals*))
        (*function-block-names* (list name)))
    (multiple-value-bind (effective-args effective-body docstring)
        (parse-extended-function lambda-list body)
      (values effective-args
              (compile-function-body effective-args effective-body)
              docstring))))

(define-statement-operator defun% (name lambda-list &rest body)
  (multiple-value-bind (effective-args body-block docstring)
      (compile-named-function-body name lambda-list body)
    (list 'ps-js:defun name effective-args docstring body-block)))

(defun maybe-rename-local-function (fun-name)
  (or (getf *local-function-names* fun-name) fun-name))

(defun collect-function-names (fn-defs)
  (loop for (fn-name) in fn-defs
        collect fn-name
        collect (if (or (member fn-name *enclosing-lexicals*)
                        (lookup-macro-def fn-name *symbol-macro-env*))
                    (ps-gensym (string fn-name))
                    fn-name)))

(defun compile-named-local-function (name args body)
  (multiple-value-bind (args1 body-block)
      (compile-named-function-body name args body)
    `(ps-js:lambda ,args1 ,body-block)))

(defmacro local-functions (special-op &body bindings)
  `(if in-function-scope?
       (let* ((fn-renames (collect-function-names fn-defs))
              ,@bindings)
         `(,(if compile-expression? 'ps-js:|,| 'ps-js:block)
            ,@definitions
            ,@(compile-progn body)))
       (ps-compile (with-lambda-scope `(,',special-op ,fn-defs ,@body)))))

(defun compile-local-function-defs (fn-defs renames)
  (loop for (fn-name . (args . body)) in fn-defs collect
       (progn (when compile-expression?
                (push (getf renames fn-name)
                      *vars-needing-to-be-declared*))
              (list (if compile-expression? 'ps-js:= 'ps-js:var)
                    (getf renames fn-name)
                    (compile-named-local-function fn-name args body)))))

(define-expression-operator flet (fn-defs &rest body)
  (local-functions flet
    ;; the function definitions need to be compiled with previous
    ;; lexical bindings
    (definitions (compile-local-function-defs fn-defs fn-renames))
    ;; the flet body needs to be compiled with the extended
    ;; lexical environment
    (*enclosing-lexicals*   (append fn-renames *enclosing-lexicals*))
    (*loop-scope-lexicals*  (when in-loop-scope?
                              (append fn-renames *loop-scope-lexicals*)))
    (*local-function-names* (append fn-renames *local-function-names*))))

(define-expression-operator labels (fn-defs &rest body)
  (local-functions labels
    (*enclosing-lexicals*   (append fn-renames *enclosing-lexicals*))
    (*loop-scope-lexicals*  (when in-loop-scope?
                              (append fn-renames *loop-scope-lexicals*)))
    (*local-function-names* (append fn-renames *local-function-names*))
    (definitions (compile-local-function-defs fn-defs *local-function-names*))))

(define-expression-operator function (fn-name)
  ;; one of the things responsible for function namespace
  (ps-compile (maybe-rename-local-function fn-name)))
