;;; Copyright 2005-2006 Henrik Hjelte
;;; Copyright 2007-2012 Vladimir Sedach

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

(in-package #:parenscript.tests)

(defun normalize-js-output (str)
  (cl-ppcre:regex-replace-all "\\s+" str " "))

(defmacro test-ps-js (testname parenscript javascript
                      &key (js-target-version *js-target-version*))
  `(fiveam:test ,testname ()
     (fiveam:is
      (string= (normalize-js-output ,javascript)
               (normalize-js-output
                (let ((*js-target-version* ,js-target-version))
                  (ps-doc* ',parenscript)))))))

(defun js-repr (x)
  (cond ((or (consp x) (simple-vector-p x))
         (cl-js:js-array
          (make-array (length x)
                      :initial-contents (map 'vector #'js-repr x)
                      :adjustable t)))
        ((null x) :null)
        (t x)))

(defmacro %test-js-eval (testname parenscript test-statement)
  `(fiveam:test ,testname ()
     (cl-js:with-js-env ()
       (let ((js-result (cl-js:run-js (ps-doc* ',parenscript))))
         ,test-statement))))

(defmacro test-js-eval (testname parenscript expected)
  `(%test-js-eval ,testname ,parenscript
     (fiveam:is (equalp js-result (js-repr ,expected)))))

(defmacro test-js-eval-epsilon (testname parenscript expected)
  `(%test-js-eval ,testname ,parenscript
     (fiveam:is (< (abs (- js-result ,expected)) 0.0001))))

(fiveam:def-suite parenscript-tests)
(fiveam:def-suite output-tests         :in parenscript-tests)
(fiveam:def-suite package-system-tests :in parenscript-tests)
(fiveam:def-suite eval-tests           :in parenscript-tests)

(defun run-tests ()
  (let ((*js-string-delimiter* #\'))
    (fiveam:run! 'parenscript-tests)))
