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


(my:with-package vi-tilde-fringe
  :ensure t
  :init (add-hook 'my:first-frame-hook
                  #'(lambda ()
                      (when (fboundp 'define-fringe-bitmap)
                        (global-vi-tilde-fringe-mode)))))


(my:with-package multiple-cursors
  :ensure t
  :init (my:kmap ("C->" #'mc/mark-next-like-this)
                 ("C-<" #'mc/mark-previous-like-this)
                 ("C-c C-<" #'mc/mark-all-like-this)))

(my:with-package expand-region
  :ensure t
  :init (progn
          (autoload 'er/mark-symbol "expand-region")
          (my:kmap ("C-=" 'er/expand-region)
                   ("C-+" 'er/mark-symbol))))

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

(my:with-package shackle
  :ensure t
  :init (shackle-mode t)
  :config
  (progn
    (add-to-list 'shackle-rules
                 '(" *NeoTree*" :align left :size 25))))

(my:with-package avy
  :ensure t
  :init (progn
          (setq-default avy-background t)
          (my:kmap "C-; p" #'avy-goto-word-1)))

(my:with-package ace-window
  :ensure t
  :init
  (progn
    (setq aw-scope 'visible
          aw-dispatch-always t
          aw-background nil)
    (my:kmap ("C-c w" "C-c C-w" #'ace-window)))
  :config
  (progn
    (defun my:aw-put-window (window)
      (my:apply-to-window
       #'my:query-move-to-window window (selected-window)))
    (fset 'aw-window-list 'my:visible-window-list)
    (setq aw-dispatch-alist
          '((?w aw-switch-to-window " Ace - Window")
            (?m my:aw-put-window " Ace - Move Window")
            (?n my:detach-window)
            (?r my:resize-window)
            (?h windmove-left) (?j windmove-down)
            (?k windmove-up) (?l windmove-right)
            (?s aw-swap-window " Ace - Swap Window")
            (?x aw-delete-window " Ace - Delete Window")
            (?c aw-split-window-fair " Ace - Split Fair Window")
            (?v aw-split-window-vert " Ace - Split Vert Window")
            (?b aw-split-window-horz " Ace - Split Horz Window")
            (?i delete-other-windows " Ace - Maximize Window")
            (?o delete-other-windows)))))


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

(my:with-package neotree
  :ensure t
  :init (progn
          (setq-default neo-mode-line-type 'none
                        neo-show-updir-line nil
                        neo-theme 'nerd
                        neo-confirm-create-file 'off-p
                        neo-confirm-create-directory 'off-p))
  :config (progn
            (defun -my:neotree-display (buffer _alist)
              (let ((win (shackle-display-buffer
                          buffer nil '(:align 'left :size 25))))
                (setq neo-global--buffer (window-buffer win)
                      neo-global--window win)))
            (setq neo-display-action '(-my:neotree-display))))


(my:with-package diff-hl
  :ensure t
  :defer t
  :init (global-diff-hl-mode))


(my:with-package projectile
  :ensure t
  :defer t
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


(my:with-package flycheck
  :ensure t
  :init (setq flycheck-indication-mode 'right-fringe)
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
                #b00000000)))))

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
