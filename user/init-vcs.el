;;; init-vcs.el --- Version control  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'init-package)
(eval-when-compile
  (require 'init-keybind))


(my:with-package diff-hl
  :ensure t
  :defer 10
  :init (progn
          (diff-hl-margin-mode t)
          (global-diff-hl-mode t)))


(my:with-package magit
  :ensure t
  :init (my:kmap
         ("<f6>" #'magit-status))
  :config (progn
            (setq-default
             ;; by-word diff
             magit-diff-refine-hunk t)))


(provide 'init-vcs)

;;; init-vcs.el ends here
