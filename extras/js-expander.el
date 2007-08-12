;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; What this code does: put your cursor at a ParenScript expression
;;;; in a ParenScript file and type 'C-c j' to bring up a buffer with
;;;; the resulting Javascript code. This feature depends on Slime (and
;;;; your ParenScript buffer being in slime-mode; it doesn't work in
;;;; slime-repl-mode, which is intentional). It is inspired (and works
;;;; like) the Slime 'C-c M-m' macroexpansion feature.

;;;; Copyright 2007, Vladimir Sedach. See the COPYING file in the
;;;; above directory for licensing information.

;;; The code below is a generic facility for adding "macroexpand-like" buffer expansion to Slime
(defun slime-eval-custom-expand (expander exp-str package buffer-name buffer-mode printer)
  (lexical-let ((package package)
                (buffer-name buffer-name)
                (buffer-mode buffer-mode)
                (printer printer))
    (slime-eval-async
     (list 'swank:eval-and-grab-output (format "(%s %s)" expander exp-str))
     (lambda (expansion)
       (with-current-buffer (slime-get-temp-buffer-create buffer-name :mode buffer-mode :reusep t)
         (setq slime-buffer-connection (slime-connection))
         (setq slime-buffer-package package)
         (setq buffer-read-only nil)
         (erase-buffer)
         (insert (funcall printer (second expansion)))
         (setq buffer-read-only t)
         (font-lock-fontify-buffer)))
     package)))

(defun* slime-add-custom-expander (key expander buffer-name &optional (buffer-mode 'slime-mode) (printer #'identity))
  (slime-define-key key (lexical-let ((expander expander)
                                      (buffer-name buffer-name)
                                      (buffer-mode buffer-mode)
                                      (printer printer))
                          (lambda (&rest _)
                            (interactive "P")
                            (slime-eval-custom-expand expander
                                                      (first (slime-sexp-at-point-for-macroexpansion))
                                                      (slime-current-package)
                                                      buffer-name
                                                      buffer-mode
                                                      printer)))
                    :prefixed t :inferior t))

;;; This actually defines the expander. If the code above belongs in slime.el, the code below would go into .emacs
(slime-add-custom-expander "j" 'ps:ps "*ParenScript generated Javascript*" 'c-mode #'read)
