;;; init-pkgs.el --- Main config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-keybind)


(my:with-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :config (progn
            (setq-default undo-tree-visualizer-timestamps t
                          undo-tree-visualizer-diff t)))

(my:with-package multiple-cursors
  :ensure t
  :init (my:kmap ("C->" #'mc/mark-next-like-this)
                 ("C-<" #'mc/mark-previous-like-this)
                 ("C-c C-<" #'mc/mark-all-like-this)))

(my:with-package expand-region
  :ensure t
  :init (progn
          (autoload 'er/mark-symbol "expand-region")
          (my:kmap ("C-=" #'er/expand-region)
                   ("C-+" #'er/mark-symbol))))

(my:with-package visual-regexp
  :ensure t
  :init (my:kmap
         ([remap query-replace-regexp] #'vr/query-replace)
         ("M-s m" #'vr/mc-mark))
  :config (progn
            (setq-default vr/auto-show-help nil)
            (my:kmap* vr/minibuffer-keymap
                      ("C-c p" nil) ;; Will be shadowed by projectile
                      ("C-c v" #'vr--shortcut-toggle-preview))))

(my:with-package iy-go-to-char
  :ensure t
  :init (my:kmap ("C-." #'iy-go-up-to-char)
                 ("C-," #'iy-go-up-to-char-backward))
  :config (setq-default
           ;; kill-region do not work with `multiple-cursors-mode'
           iy-go-to-char-override-local-map nil))

(my:with-package avy
  :ensure t
  :init (progn
          (setq-default avy-background t)
          (my:kmap "C-; p" #'avy-goto-word-1)))

(my:with-package ace-window
  :ensure t
  :init
  (progn
    (require 'init-wm)
    (autoload 'aw-select "ace-window")
    (defun my:aw-put-window (window)
      (my:apply-to-window
       #'my:query-move-to-window window (selected-window)))
    (defun my:ace-move-window ()
      (interactive)
      (aw-select " Ace - Move Window" #'my:aw-put-window))
    (my:kmap ("C-c o" #'ace-window)
             ("C-c C-o m" #'my:ace-move-window)
             ("C-c C-o s" #'ace-swap-window)
             ("C-c C-o d" #'ace-delete-window))))


;; External tools
(my:with-package ag
  :ensure t
  :config (setq-default ag-highlight-search t))

(my:with-package magit
  :ensure t
  :init (my:kmap
         ("<f6>" #'magit-status))
  :config (setq-default
           ;; by-word diff
           magit-diff-refine-hunk t))

;; Project management and project tree
(my:with-package dired+
  :ensure t
  :init (progn
          (setq-default
           diredp-hide-details-initially-flag t
           diredp-hide-details-propagate-flag t)
          (diredp-toggle-find-file-reuse-dir t)))

(my:with-package diff-hl
  :ensure t
  :defer t
  :init (global-diff-hl-mode))

(my:with-package projectile
  :ensure t
  :init (progn
          (setq-default projectile-completion-system 'ivy)
          (projectile-global-mode))
  :config (progn
            ;; Try to emulate ede (from CEDET) project
            (with-eval-after-load 'semanticdb
              (setq-default semanticdb-project-root-functions
                            projectile-project-root-files-functions))))


(my:with-package flycheck
  :ensure t)

(my:with-package js2-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.json" . js-mode)))

(my:with-package yaml-mode
  :ensure t)

(my:with-package lua-mode
  :ensure t
  :config (setq-default lua-indent-level 4))

(my:with-package rust-mode
  :ensure t)

(provide 'init-pkgs)

;;; init-pkgs.el ends here
