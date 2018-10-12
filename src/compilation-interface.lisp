;;; Copyright 2005 Manuel Odendahl
;;; Copyright 2005-2006 Edward Marco Baringer
;;; Copyright 2006 Luca Capello
;;; Copyright 2007-2009 Red Daly
;;; Copyright 2008 Travis Cross
;;; Copyright 2007-2011 Vladimir Sedach
;;; Copyright 2009-2010 Daniel Gackle

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

(defparameter *js-target-version* "1.3")

(defvar *parenscript-stream* nil)

(defmacro ps (&body body)
  "Given Parenscript forms (an implicit progn), compiles those forms
to a JavaScript string at macro-expansion time. Expands into a form
which evaluates to a string."
  (let ((printed-forms (parenscript-print
                        (compile-statement `(progn ,@body))
                        nil)))
    (if (and (not (cdr printed-forms))
             (stringp (car printed-forms)))
        (car printed-forms)
        (let ((s (gensym)))
          `(with-output-to-string (,s)
             ,@(mapcar (lambda (x) `(write-string ,x ,s))
                       printed-forms))))))

(defmacro ps-to-stream (stream &body body)
  "Given Parenscript forms (an implicit progn), compiles those forms
to a JavaScript string at macro-expansion time. Expands into a form
which writes the resulting code to stream."
  (let ((printed-forms (parenscript-print
                        (compile-statement `(progn ,@body))
                        nil)))
    `(let ((*parenscript-stream* ,stream))
       ,@(mapcar (lambda (x) `(write-string ,x *parenscript-stream*))
                 printed-forms))))

(defun ps* (&rest body)
  "Compiles body to a JavaScript string. If *parenscript-stream* is
bound, writes the output to *parenscript-stream*, otherwise returns a
string."
  (let ((*psw-stream* (or *parenscript-stream* (make-string-output-stream))))
    (parenscript-print (compile-statement `(progn ,@body)) t)
    (unless *parenscript-stream*
      (get-output-stream-string *psw-stream*))))

(defmacro with-blank-compilation-environment (&body body)
  `(let ((*ps-gensym-counter* 0)
         (*special-variables* nil))
     ,@body))

(defmacro ps-doc (&body body)
  "Expands Parenscript forms in a clean environment."
  (with-blank-compilation-environment
    (macroexpand-1 `(ps ,@body))))

(defun ps-doc* (&rest body)
  (with-blank-compilation-environment
    (apply #'ps* body)))

(defvar *js-inline-string-delimiter* #\"
  "Controls the string delimiter char used when compiling Parenscript in ps-inline.")

(defun ps-inline* (form &optional
                   (*js-string-delimiter* *js-inline-string-delimiter*))
  (concatenate 'string "javascript:" (ps* form)))

(defmacro+ps ps-inline (form &optional
                             (string-delimiter *js-inline-string-delimiter*))
  `(concatenate 'string "javascript:"
                ,@(let ((*js-string-delimiter* string-delimiter))
                    (parenscript-print (compile-statement form) nil))))

(defvar *ps-read-function* #'read)

(defun ps-compile-stream (stream)
  "Reads (using the value of *ps-read-function*, #'read by default, as
the read function) Parenscript forms from stream and compiles them as
if by ps*. If *parenscript-stream* is bound, writes the output to
*parenscript-stream*, otherwise and returns a string."
  (let ((output-stream (or *parenscript-stream* (make-string-output-stream))))
    (let ((*compilation-level* :toplevel)
          (*readtable* *readtable*)
          (*package* *package*)
          (*parenscript-stream* output-stream)
          (eof '#:eof))
      (loop for form = (funcall *ps-read-function* stream nil eof)
            until (eq form eof) do (ps* form) (fresh-line *parenscript-stream*)))
    (unless *parenscript-stream*
      (get-output-stream-string output-stream))))

(defun ps-compile-file (source-file &key (element-type 'character) (external-format :default))
  "Opens file as input stream and calls ps-compile-stream on it."
  (with-open-file (stream source-file
                          :direction :input
                          :element-type element-type
                          :external-format external-format)
    (ps-compile-stream stream)))
