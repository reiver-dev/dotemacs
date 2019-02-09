;;; init-pkgs.el --- Main config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(eval-when-compile
  (require 'init-package)
  (require 'init-keybind))


(my:with-package undo-tree
  :ensure t
  :defer 1
  :init (global-undo-tree-mode)
  :config (progn
            (setq-default undo-tree-visualizer-timestamps t
                          undo-tree-visualizer-diff t)))


(my:with-package multiple-cursors
  :ensure t
  :init (my:kmap ("M-i" #'mc/mark-next-like-this)
                 ("M-I" #'mc/mark-previous-like-this)
                 ("C-x i" #'mc/mark-all-like-this)))


(my:with-package expand-region
  :ensure t
  :init (progn
          (autoload 'er/mark-symbol "expand-region")
          (my:kmap ("M-]" 'er/expand-region)
                   ("M-}" 'er/contract-region)
                   ("C-x p p" 'er/mark-symbol))))


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
  :init (my:kmap ("M-z" #'iy-go-up-to-char)
                 ("M-Z" #'iy-go-up-to-char-backward))
  :config (setq-default
           ;; kill-region do not work with `multiple-cursors-mode'
           iy-go-to-char-override-local-map nil))


(my:with-package avy
  :ensure t
  :init (progn
          (setq-default avy-background t
                        avy-all-windows 'all-frames)
          (my:kmap "C-o C-o" #'avy-goto-word-1)))


(my:with-package which-key
  :ensure t
  :init (my:after-init #'which-key-mode)
  :config (progn
            (setq which-key-sort-order #'which-key-prefix-then-key-order
                  which-key-sort-uppercase-first nil
                  which-key-add-column-padding 1
                  which-key-max-display-columns nil
                  which-key-min-display-lines 5)
            (which-key-setup-side-window-bottom)))


;; External tools
(my:with-package ag
  :ensure t
  :config (setq-default ag-highlight-search t))


(my:with-package rg
  :ensure t
  :config
  (progn
    (my:kmap* rg-mode-map
              ("p" #'compilation-previous-error)
              ("n" #'compilation-next-error))))


(my:with-package magit
  :ensure t
  :init (my:kmap
         ("<f6>" #'magit-status))
  :config (setq-default
           ;; by-word diff
           magit-diff-refine-hunk t))

;; Project management and project tree

(my:with-package neotree
  :disable t
  :ensure t
  :init (progn
          (setq-default neo-mode-line-type 'none
                        neo-show-updir-line nil
                        neo-theme 'nerd
                        neo-confirm-create-file 'off-p
                        neo-confirm-create-directory 'off-p))
  :config (my:after schackle
            (defun -my:neotree-display (buffer _alist)
              (let ((win (shackle-display-buffer
                          buffer nil '(:align 'left :size 25))))
                (setq neo-global--buffer (window-buffer win)
                      neo-global--window win)))
            (setq neo-display-action '(-my:neotree-display))))


(my:with-package diff-hl
  :ensure t
  :defer 10
  :init (progn
          (diff-hl-margin-mode t)
          (global-diff-hl-mode t)))


(my:with-package projectile
  :ensure t
  :defer 10
  :init (progn
          (setq-default projectile-completion-system 'ivy
                        projectile-switch-project-action
                        #'projectile-commander)
          (projectile-mode))
  :config (progn
            ;; Try to emulate ede (from CEDET) project
            (my:after semanticdb
              (setq-default semanticdb-project-root-functions
                            projectile-project-root-files-functions))))


(my:with-package counsel-projectile
  :ensure t
  :init (my:after projectile
          (counsel-projectile-mode t)))


(my:with-package pkg-info
  :ensure t
  :init (autoload 'pkg-info-version-info "pkg-info"))


(my:with-package flycheck
  :ensure t
  :init (progn
          (setq flycheck-indication-mode 'right-fringe
                flycheck-check-syntax-automatically '(save mode-enabled))
          (defun turn-on-flycheck-mode ()
            "Unconditionally enable `flycheck-mode'"
            (flycheck-mode 1))
          (defun turn-off-flycheck-mode ()
            "Unconditionally disable `flycheck-mode'"
            (flycheck-mode -1)))
  :config
  (progn
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011001
                #b00110110
                #b01101100
                #b11011000
                #b01101100
                #b00110110
                #b00011001
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000)))
    (add-hook 'my:exporting-hook #'turn-off-flycheck-mode)))


(my:with-package js2-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.json" . js-mode)))


(my:with-package yaml-mode
  :ensure t)


(my:with-package lua-mode
  :ensure t
  :config (setq-default lua-indent-level 4))


(provide 'init-pkgs)

;;; init-pkgs.el ends here
