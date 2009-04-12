(in-package "PARENSCRIPT")

;; Utilities for accessing standard DOM functionality in a Lispier, PSier way.

(defpsmacro inner-html (el)
  `(@ ,el :inner-h-t-m-l))

(defpsmacro uri-encode (str)
  `(if (null ,str) "" (encode-u-r-i-component ,str)))

(defpsmacro attribute (el attr)
  `((@ ,el :get-attribute) ,attr))

(defun assert-is-one-of (val options)
  (unless (member val options)
    (error "~s is not one of ~s" val options)))

(defpsmacro offset (what el)
  (if (consp what)
      `(offset ,(eval what) ,el)
      (progn (assert-is-one-of what '(:top :left :height :width :bottom :right))
             (if (member what '(:top :left :height :width))
                 `(@ ,el ,(intern (format nil "OFFSET-~a" what)))
                 (aif (assoc what '((:bottom :top :height) (:right :left :width)))
                      `(+ (offset ,(second it) ,el) (offset ,(third it) ,el)))))))

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
