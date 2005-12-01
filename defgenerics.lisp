(in-package :js)

(defgeneric js-equal (obj1 obj2)
  (:documentation "Determine if two enscript-javascript statements are equivalent"))

(defgeneric js-to-strings (expression start-pos)
  (:documentation "Transform an enscript-javascript expression to a string"))

(defgeneric js-to-statement-strings (code-fragment start-pos)
  (:documentation "Transform an enscript-javascript code fragment to a string"))

(defgeneric expression-precedence (expression)
  (:documentation "Returns the precedence of an enscript-javascript expression"))

(defgeneric function-start-string (function)
  (:documentation "Returns the string that starts the function - this varies according to whether
this is a lambda or a defun"))
