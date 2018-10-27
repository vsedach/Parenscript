;;; Copyright 2005 Manuel Odendahl
;;; Copyright 2005-2006 Edward Marco Baringer
;;; Copyright 2007 Attila Lendvai
;;; Copyright 2007 Red Daly
;;; Copyright 2007-2012 Vladimir Sedach
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

(let ((cache (make-hash-table :test 'equal)))
  (defun encode-js-identifier (identifier)
    "Given a string, produces to a valid JavaScript identifier by
following transformation heuristics case conversion. For example,
paren-script becomes parenScript, *some-global* becomes SOMEGLOBAL."
    (when (and (not (string= identifier "[]"))
               (find #\[ identifier))
      (warn "Parenscript symbol ~A contains a literal array accessor.
This compound naming convention is deprecated and will be removed!
Use AREF, ELT, GETPROP, @, or CHAIN instead."
            identifier))
    (when (find #\. identifier)
      (warn "Parenscript symbol ~A contains one or more dot operators.
This compound naming convention is deprecated and will be removed!
Use GETPROP, @, or CHAIN instead."
            identifier))
    (or
     (gethash identifier cache)
     (setf
       (gethash identifier cache)
       (cond
          ((some (lambda (c) (find c "-*+!?#@%/=:<>^")) identifier)
            (let ((lowercase     t)
                  (all-uppercase nil))
              (acond
                ((nth-value 1
                   (cl-ppcre:scan-to-strings
                    "[\\*|\\+](.+)[\\*|\\+](.*)"
                    identifier :sharedp t))
                 (setf all-uppercase t
                       identifier (concatenate
                                   'string (aref it 0) (aref it 1))))
                ((and (> (length identifier) 1)
                      (or (eql (char identifier 0) #\+)
                          (eql (char identifier 0) #\*)))
                 (setf lowercase  nil
                       identifier (subseq identifier 1))))
              (with-output-to-string (acc)
                (loop
                   for c across identifier
                   do (acond
                       ((eql c #\-)
                        (setf lowercase (not lowercase)))
                       ((position c "!?#@%+*/=:<>^")
                        (write-sequence
                         (aref #("bang" "what" "hash" "at" "percent"
                                 "plus" "star" "slash" "equals" "colon"
                                 "lessthan" "greaterthan" "caret")
                               it)
                         acc))
                       (t
                        (write-char
                         (if (and lowercase (not all-uppercase))
                             (char-downcase c)
                             (char-upcase c))
                         acc)
                        (setf lowercase t)))))))
          (#.(eql :invert (readtable-case
                           (named-readtables:find-readtable :parenscript)))
             (cond
               ((every #'upper-case-p
                       (remove-if-not #'alpha-char-p identifier))
                (string-downcase identifier))
               ((every #'lower-case-p
                       (remove-if-not #'alpha-char-p identifier))
                (string-upcase identifier))
               (t identifier)))
          (t identifier))))))

(defun ordered-set-difference (list1 list2 &key (test #'eql))
  "CL set-difference may not preserve order."
  (reduce (lambda (list el) (remove el list :test test))
          (cons list1 list2)))

(defun flatten (x &optional acc)
  (cond ((null x) acc)
        ((atom x) (cons x acc))
        (t (flatten (car x) (flatten (cdr x) acc)))))

(defun tree-find (A tree)
  (or (equal A tree)
      (when (consp tree)
        (loop for x on tree thereis
              (or (tree-find A (car x))
                  (unless (listp (cdr x))
                    (equal A (cdr x))))))))

(defun parse-semver (semver-string)
  (let ((semver-list   (cl-ppcre:split "\\." semver-string))
        (semver-scaled 0))
    (dotimes (i 3)
      (incf semver-scaled
            (* (expt 1000 (- 2 i))
               (parse-integer (or (nth i semver-list) "0")))))
    semver-scaled))

(defun js-target-at-least (version)
  (>= (parse-semver *js-target-version*) (parse-semver version)))
