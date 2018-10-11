;;; Copyright 2005 Manuel Odendahl
;;; Copyright 2005 Edward Marco Baringer
;;; Copyright 2007-2011 Vladimir Sedach

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

(defvar *ps-html-empty-tag-aware-p* t)
(defvar *ps-html-mode* :sgml "One of :sgml or :xml")

(defvar *html-empty-tags* '(:area :atop :audioscope :base :basefont :br :choose :col :frame
                            :hr :img :input :isindex :keygen :left :limittext :link :meta
                            :nextid :of :over :param :range :right :spacer :spot :tab :wbr))

(defun empty-tag-p (tag)
  (and *ps-html-empty-tag-aware-p*
       (member tag *html-empty-tags*)))

(defun concat-constant-strings (str-list)
  (flet ((expand (expr)
           (setf expr (ps-macroexpand expr))
           (cond ((and (consp expr) (eq (car expr) 'quote) (symbolp (second expr)))
                  (symbol-to-js-string (second expr)))
                 ((keywordp expr) (string-downcase expr))
                 ((characterp expr) (string expr))
                 (t expr))))
    (reverse (reduce (lambda (optimized-list next-expr)
                       (let ((next-obj (expand next-expr)))
                         (if (and (or (numberp next-obj) (stringp next-obj))
                                  (stringp (car optimized-list)))
                             (cons (format nil "~a~a" (car optimized-list) next-obj) (cdr optimized-list))
                             (cons next-obj optimized-list))))
                     (cons () str-list)))))

(defun process-html-forms-lhtml (forms)
  (let ((r ()))
    (labels ((process-attrs (attrs)
               (do (attr-test attr-name attr-val)
                   ((not attrs))
                 (setf attr-name (pop attrs)
                       attr-test (when (not (keywordp attr-name))
                                   (let ((test attr-name))
                                     (setf attr-name (pop attrs))
                                     test))
                       attr-val (pop attrs))
                 (if attr-test
                     (push `(if ,attr-test
                                (stringify ,(format nil " ~A=\"" attr-name) ,attr-val "\"")
                                "")
                           r)
                     (progn
                       (push (format nil " ~A=\"" attr-name) r)
                       (push attr-val r)
                       (push "\"" r)))))
             (process-form% (tag attrs content)
               (push (format nil "<~A" tag) r)
               (process-attrs attrs)
               (if (or content (not (empty-tag-p tag)))
                   (progn (push ">" r)
                          (map nil #'process-form content)
                          (push (format nil "</~A>" tag) r))
                   (progn (when (eql *ps-html-mode* :xml)
                            (push "/" r))
                          (push ">" r))))
             (process-form (form)
               (cond ((keywordp form) (process-form (list form)))
                     ((atom form) (push form r))
                     ((and (consp form) (keywordp (car form)))
                      (process-form% (car form) () (cdr form)))
                     ((and (consp form) (consp (first form)) (keywordp (caar form)))
                      (process-form% (caar form) (cdar form) (cdr form)))
                     (t (push form r)))))
      (map nil #'process-form forms)
      (concat-constant-strings (reverse r)))))

(defun process-html-forms-cl-who (forms)
  (let ((r ()))
    (labels ((process-form (form)
               (cond ((keywordp form) (process-form (list form)))
                     ((atom form) (push form r))
                     ((and (consp form) (keywordp (car form)))
                      (push (format nil "<~A" (car form)) r)
                      (labels ((process-attributes (el-body)
                                 (when el-body
                                   (if (keywordp (car el-body))
                                       (progn
                                         (push (format nil " ~A=\""
                                                       (car el-body)) r)
                                         (push (cadr el-body) r)
                                         (push "\"" r)
                                         (process-attributes (cddr el-body)))
                                       el-body))))
                        (let ((content (process-attributes (cdr form))))
                          (if (or content (not (empty-tag-p (car form))))
                              (progn (push ">" r)
                                     (when content (map nil #'process-form content))
                                     (push (format nil "</~A>" (car form)) r))
                              (progn (when (eql *ps-html-mode* :xml)
                                       (push "/" r))
                                     (push ">" r))))))
                     (t (push form r)))))
      (map nil #'process-form forms)
      (concat-constant-strings (reverse r)))))

(defmacro+ps ps-html (&rest html-forms)
  `(stringify ,@(with-standard-io-syntax (process-html-forms-lhtml html-forms))))

(defmacro+ps who-ps-html (&rest html-forms)
  `(stringify ,@(with-standard-io-syntax (process-html-forms-cl-who html-forms))))
