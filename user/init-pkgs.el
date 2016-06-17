;;; init-pkgs.el --- Main config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-keybind)

(defconst my:user-dir (file-name-directory
                       (or load-file-name buffer-file-name)))
(defconst my:snippets-dir (expand-file-name "snippets" my:user-dir))


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

(defvar my:paredit-extended-mode-map (make-sparse-keymap)
  "Keymap for `my:paredit-exteded-mode'")
(define-minor-mode my:paredit-extended-mode
  "Sets from `smartparens-mode': \\{my:paredit-extended-mode-map}"
  :keymap my:paredit-extended-mode-map)

(my:with-package smartparens
  :ensure t
  :init (require 'smartparens-config)
  :config (progn
            (setq-default
             ;; disable overlay
             sp-highlight-pair-overlay nil
             sp-highlight-wrap-overlay nil
             sp-highlight-wrap-tag-overlay nil)
            (add-to-list 'sp-ignore-modes-list 'my:large-file-mode)
            (smartparens-global-mode t)
            (show-smartparens-global-mode t)
            (my:kmap* smartparens-mode-map
                      ;; Basic movements
                      ("C-M-f" #'sp-forward-sexp)
                      ("C-M-b" #'sp-backward-sexp)
                      ("C-M-d" #'sp-down-sexp)
                      ("C-M-u" #'sp-backward-up-sexp)
                      ("C-M-p" #'sp-backward-down-sexp) ; remap backward-list
                      ("C-M-n" #'sp-up-sexp) ; remap forward-list
                      ("C-M-k" #'sp-kill-sexp)
                      ;; List manipulation
                      ("C-x p c" #'sp-splice-sexp)
                      ("C-x p s" #'sp-split-sexp)
                      ("C-x p j" #'sp-join-sexp)
                      ("C-x p a" #'sp-splice-sexp-killing-around)
                      ("C-x p b" #'sp-splice-sexp-killing-backward)
                      ("C-x p f" #'sp-splice-sexp-killing-forward)
                      ("C-x p r" #'sp-rewrap-sexp)
                      ("C-x p u" #'sp-unwrap-sexp)
                      ("C-x p d" #'sp-backward-unwrap-sexp)
                      ("C-x p w" #'sp-swap-enclosing-sexp)
                      ("C-x p p" #'sp-select-next-thing-exchange))
            (my:kmap* my:paredit-extended-mode-map
                      ("C-M-t" #'sp-transpose-sexp) ; remap transpose-sexps
                      ;; Direction manipulation
                      ("M-<up>"      #'sp-splice-sexp-killing-backward)
                      ("M-<down>"    #'sp-splice-sexp-killing-forward)
                      ("C-<right>"   #'sp-forward-slurp-sexp)
                      ("C-<left>"    #'sp-forward-barf-sexp)
                      ("C-M-<left>"  #'sp-backward-slurp-sexp)
                      ("C-M-<right>" #'sp-backward-barf-sexp))
            (defun my:lisp-setup-paredit ()
              (my:paredit-extended-mode t)
              (smartparens-strict-mode))
            (add-hook 'lisp-mode-hook #'my:lisp-setup-paredit)
            (add-hook 'emacs-lisp-mode-hook #'my:lisp-setup-paredit)))

(my:with-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

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


(my:with-package ivy
  :ensure t
  :init (ivy-mode t))


(my:with-package counsel
  :ensure t
  :init (counsel-mode t)
  :config (my:kmap*
           counsel-mode-map
           ([remap yank-pop] nil)
           ("C-M-y" #'counsel-yank-pop)))


;; Completion
(my:with-package company
  :ensure t
  :init (global-company-mode t)
  :config (progn
            (my:kmap* company-mode-map
                      ("C-<tab>" #'company-complete))
            (my:kmap* company-active-map
                      ("C-p" #'company-select-previous)
                      ("C-n" #'company-select-next))
            (my:kmap "C-M-/" #'company-files)
            (setq-default company-tooltip-limit 20
                          ;; Put semantic backend on separate key
                          company-backends
                          (remove 'company-semantic company-backends))
            (with-eval-after-load 'semantic
              (defun my:company-semantic-setup ()
                "Sets `company-semantic' keybind locally"
                (local-set-key (kbd "C-<return>") #'company-semantic))
              (add-hook 'c-mode-hook #'my:company-semantic-setup)
              (add-hook 'c++-mode-hook #'my:company-semantic-setup))))

(my:with-package yasnippet
  :ensure t
  :defer t
  :init (progn
          (unless (file-directory-p my:snippets-dir)
            (make-directory my:snippets-dir))
          (yas-global-mode t))
  :config (progn
            ;; No more toolkit popups
            (setq-default
             yas-prompt-functions
             '(yas-ido-prompt yas-completing-prompt yas-no-prompt))
            ;; Just custom snippet dir
            (add-to-list 'yas-snippet-dirs my:snippets-dir)
            (with-eval-after-load 'smartparens
              (advice-add #'yas-expand :before
                          #'(lambda ()
                              "Escape from `smartparens-mode' overlay"
                              (let ((times 5))
                                (while (and (> times 0)
                                            (sp--get-active-overlay))
                                  (sp-remove-active-pair-overlay)
                                  (setq times (- times 1)))))))
            (add-hook 'term-mode-hook
                      (lambda () (yas-minor-mode -1)))))

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

(my:with-package ggtags
  :if (executable-find "gtags")
  :ensure t
  :init (progn
          (defun my:turn-on-ggtags-mode ()
            "Set `ggtags-mode' on"
            (ggtags-mode t))
          (add-hook 'c-mode-hook #'my:turn-on-ggtags-mode)
          (add-hook 'c++-mode-hook #'my:turn-on-ggtags-mode))
  :config (my:kmap* ggtags-mode-map
                    ("M-." "C-M-." "M-*" "M-," nil)
                    ("M-." #'ggtags-find-tag-dwim)
                    ("C-M-." #'ggtags-find-tag-regexp)))

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


;; Language specific
(my:with-package anaconda-mode
  :ensure t
  :init (add-hook 'python-mode-hook #'anaconda-mode)
  :config (progn
            (my:kmap* anaconda-mode-map
                      ("M-*" "M-," "M-." "C-M-i" nil)
                      ("M-." #'anaconda-mode-find-definitions)
                      ("M-," #'anaconda-mode-go-back)
                      ("C-M-." #'anaconda-mode-find-assignments)
                      ("M-]" #'anaconda-mode-find-references)
                      ([remap completion-at-point] #'anaconda-mode-complete))))

(my:with-package company-anaconda
  :ensure t
  :init (with-eval-after-load 'anaconda-mode
          (add-to-list 'company-backends #'company-anaconda)))

(my:with-package company-c-headers
  :ensure t
  :config (progn
            ;; Get include path from Semantic
            (with-eval-after-load 'semantic/dep
              (setq company-c-headers-path-system #'my:system-include-path))
            (add-to-list 'company-backends #'company-c-headers)))

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

(my:with-package haskell-mode
  :ensure t
  :config (progn
            (add-hook 'haskell-mode-hook #'haskell-indentation-mode)))

(my:with-package company-ghc
  :if (executable-find "ghc-mod")
  :ensure t
  :config (progn
            (add-to-list 'company-backends #'company-ghc)
            (add-hook 'haskell-mode-hook #'ghc-init)))

(my:with-package irony
  :if (executable-find "clang")
  :ensure t
  :init (progn
          (add-hook 'c++-mode-hook 'irony-mode)
          (add-hook 'c-mode-hook 'irony-mode))
  :config (progn
            (my:with-package company-irony
              :ensure t
              :init (add-to-list 'company-backends
                                 (list #'company-c-headers #'company-irony)))
            (my:with-package flycheck-irony
              :ensure t
              :init (with-eval-after-load 'flycheck
                      (flycheck-irony-setup)))
            (my:with-package irony-eldoc
              :ensure t
              :init (add-hook 'irony-mode-hook #'irony-eldoc))))

(my:with-package clojure-mode
  :ensure t
  :config (add-hook 'clojure-mode-hook #'my:lisp-setup-paredit))

(my:with-package cider
  :if (executable-find "lein")
  :ensure t
  :config (progn
            (add-hook 'cider-repl-mode-hook #'my:lisp-setup-paredit)))

(provide 'init-pkgs)

;;; init-pkgs.el ends here
