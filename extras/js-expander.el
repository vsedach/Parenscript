;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This is an extension to SLIME that is inspired by (and works
;;;; like) the SLIME 'C-c M-m' macroexpansion feature.

;;;; After loading, 'C-c j' (PS) or 'C-c d' (PS-DOC) at a ParenScript
;;;; expression in a slime-mode buffer will bring up a buffer with the
;;;; resulting Javascript code. Note that the extension does not work
;;;; in slime-repl-mode, which is intentional.

;;;; Copyright 2007, Vladimir Sedach. See the COPYING file in the
;;;; Parenscript distribution for licensing information.

;;; The code below is a generic facility for adding "macroexpand-like" buffer expansion to Slime
(defun slime-eval-custom-expand (expander exp-str package buffer-name buffer-mode printer)
  (lexical-let ((package package)
                (buffer-name buffer-name)
                (buffer-mode buffer-mode)
                (printer printer))
    (slime-eval-async
     (list 'swank:eval-and-grab-output (format "(%s %s)" expander exp-str))
     (lambda (expansion)
       (slime-with-popup-buffer (buffer-name)
         (funcall buffer-mode)
         (setq buffer-read-only nil)
         (erase-buffer)
         (insert (funcall printer (second expansion)))
         (setq buffer-read-only t)
         (font-lock-fontify-buffer)))
     package)))

(defun* slime-add-custom-expander (key expander buffer-name &optional (buffer-mode 'slime-mode) (printer #'identity))
  (define-key slime-parent-map (concat "\C-c" key)
    (lexical-let ((expander expander)
                  (buffer-name buffer-name)
                  (buffer-mode buffer-mode)
                  (printer printer))
      (lambda (&rest _)
        (interactive "P")
        (slime-eval-custom-expand expander
                                  (slime-sexp-at-point)
                                  (slime-current-package)
                                  buffer-name
                                  buffer-mode
                                  printer)))))

;;; This actually defines the expander. If the code above belongs in slime.el, the code below would go into .emacs
(map nil (lambda (x)
           (slime-add-custom-expander (car x)
                                      (cdr x)
                                      "*Parenscript generated Javascript*"
                                      (if (featurep 'javascript-mode) 'javascript-mode 'c-mode)
                                      #'read))
     '(("j" . ps:ps) ("d" . ps:ps-doc)))
