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

;; Utilities for accessing standard DOM functionality in a Lispier, PSier way.

(defpsmacro inner-html (el)
  `(@ ,el 'inner-h-t-m-l))

(defpsmacro uri-encode (str)
  `(if (null ,str) "" (encode-u-r-i-component ,str)))

(defpsmacro attribute (el attr)
  `((@ ,el 'get-attribute) ,attr))

(defun assert-is-one-of (val options)
  (unless (member val options)
    (error "~s is not one of ~s" val options)))

(defpsmacro offset (what el)
  (if (consp what)
      `(offset ,(eval what) ,el)
      (case what
        ((:top :left :height :width) `(@ ,el ,(intern (format nil "OFFSET-~a" what))))
        (:right `(+ (offset :left ,el) (offset :width ,el)))
        (:bottom `(+ (offset :top ,el) (offset :height ,el)))
        (:hcenter `(+ (offset :left ,el) (/ (offset :width ,el) 2)))
        (:vcenter `(+ (offset :top ,el) (/ (offset :height ,el) 2)))
        (t (error "The OFFSET macro doesn't accept ~s as a key." what)))))

(defpsmacro scroll (what el)
  (assert-is-one-of what '(:top :left :right :bottom :width :height))
  (cond ((member what '(:top :left :width :height))
         `(@ ,el ,(intern (format nil "SCROLL-~a" what))))
        ((eq what :right)
         `(+ (scroll :left ,el) (offset :width ,el)))
        ((eq what :bottom)
         `(+ (scroll :top ,el) (offset :height ,el)))))

(defpsmacro inner (what el)
  (assert-is-one-of what '(:width :height))
  `(@ ,el ,(intern (format nil "INNER-~a" what))))

(defpsmacro client (what el)
  (assert-is-one-of what '(:width :height))
  `(@ ,el ,(intern (format nil "CLIENT-~a" what))))
