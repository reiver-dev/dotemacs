;;; init-compile.el --- Eshell config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-keybind)


(setq-default
 compilation-always-kill t             ;; kill process before starting new one
 compilation-ask-about-save nil        ;; save everything
 compilation-scroll-output 'next-error ;; stop on error
 compilation-skip-threshold 2)         ;; skip warnings


(defun my:compile (&optional comint)
  "Compile without confirmation.
If used with a COMINT argument, use `comint-mode'."
  (interactive "P")
  ;; Do the command without a prompt.
  (save-window-excursion
    (compile (eval compile-command) (and comint t)))
  ;; Create a compile window of the desired width.
  (pop-to-buffer (get-buffer "*compilation*"))
  (enlarge-window (- (frame-width) 105 (window-width)) 'horizontal))


(autoload 'my:compile "compile")


(my:kmap* prog-mode-map
          ("<f8>" #'my:compile)
          ("C-<f8>" #'compile))


(defun -my:compile-mock-fringes (&optional _window)
  "Always return list of (-1 -1 nil).
This is to override `window-fringes.'"
  (list -1 -1 nil))


(defun -my:compile-set-window-nojump (proc &rest args)
  "Call PROC with ARGS while wrapping `window-fringes'."
  (let ((old (symbol-function 'window-fringes)))
    (unwind-protect
        (progn (fset 'window-fringes #'-my:compile-mock-fringes)
               (apply proc args))
      (fset 'window-fringes old))))


(my:after compile
  (my:kmap* compilation-shell-minor-mode-map
            ("<f8>" "<C-<f8>" #'recompile))
  (advice-add 'compilation-goto-locus :around
              #'-my:compile-set-window-nojump))


(provide 'init-compile)

;;; init-compile.el ends here
