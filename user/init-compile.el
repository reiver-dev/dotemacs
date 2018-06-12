;;; init-compile.el --- Eshell config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-package)
  (require 'init-keybind))


(setq-default
 compilation-always-kill t             ;; kill process before starting new one
 compilation-ask-about-save nil        ;; save everything
 compilation-scroll-output 'next-error ;; stop on error
 compilation-skip-threshold 2)         ;; skip warnings

(defun my:compile (comint)
      "Compile without confirmation.
With a prefix argument, use `comint-mode'."
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

(my:after compile
  (my:kmap* compilation-shell-minor-mode-map
            ("<f8>" "<C-<f8>" #'recompile)))


(provide 'init-compile)

;;; init-compile.el ends here
