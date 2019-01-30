;;; Copyright 2007-2010 Vladimir Sedach
;;; Copyright 2008 Travis Cross

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

(defvar *obfuscated-packages* (make-hash-table))

(defun obfuscate-package (package-designator &optional symbol-map)
  (setf (gethash (find-package package-designator)
                 *obfuscated-packages*)
        (or symbol-map
            (let ((symbol-table (make-hash-table)))
              (lambda (symbol)
                (or (gethash symbol symbol-table)
                    (setf (gethash symbol symbol-table)
                          (ps-gensym 'g))))))))

(defun unobfuscate-package (package-designator)
  (remhash (find-package package-designator) *obfuscated-packages*))

(defun maybe-obfuscate-symbol (symbol)
  (if (aand (symbol-package symbol) (eq :external (nth-value 1 (find-symbol (symbol-name symbol) it))))
      symbol
      (aif (gethash (symbol-package symbol) *obfuscated-packages*)
           (funcall it symbol)
           symbol)))

(defvar *package-prefix-table* (make-hash-table))

(defmacro ps-package-prefix (package)
  `(gethash (find-package ,package) *package-prefix-table*))

(defun symbol-to-js-string (symbol &optional (mangle-symbol-name? t))
  (let* ((symbol-name (symbol-name (maybe-obfuscate-symbol symbol)))
         (identifier (if mangle-symbol-name?
                         (encode-js-identifier symbol-name)
                         symbol-name)))
    (aif (ps-package-prefix (symbol-package symbol))
         (concatenate 'string it identifier)
         identifier)))
