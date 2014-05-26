;;  MAIN  ;;

;; my folders ;;
(defconst user-emacs-directory-full (expand-file-name user-emacs-directory))

(defconst my:user-dir (concat user-emacs-directory-full "user/"))
(defconst my:modules-dir (concat my:user-dir "modules/"))

(defconst my:backup-dir (concat user-emacs-directory-full "backup/"))
(defconst my:autosave-dir (concat user-emacs-directory-full "autosave/"))
(defconst my:snippets-dir (concat my:user-dir "snippets/"))

(dolist (dir (list my:backup-dir my:autosave-dir my:modules-dir my:snippets-dir))
  (unless (file-directory-p dir)
    (make-directory dir)))

;; package dir ;;
(let ((default-directory my:modules-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))


;; python ;;
(setq python-path
	  (concat (getenv "PYTHONPATH") ":" (concat my:user-dir "python-path")))
(setenv "PYTHONPATH" python-path)

;; backup and autosave ;;
(setq backup-directory-alist
      `((".*" . ,my:backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,my:autosave-dir t)))

;; Appearance
;; toolbars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist
             '(font . "Meslo LG S 10"))

(require 'package)
(add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(blink-cursor-mode -1)

(setq-default inhibit-startup-message t
              color-theme-is-global t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll
(setq-default mouse-wheel-scroll-amount '(3 ((shift) . 1)) ;; three line at a time
              mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
              mouse-wheel-follow-mouse t) ;; scroll window under mouse

;; Text behavior
(setq-default shift-select-mode nil
              truncate-lines nil
              word-wrap t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Indentation
(electric-indent-mode t)
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 120) 

;; Line numbers and fringe
(require 'linum)
(add-hook 'prog-mode-hook 'linum-mode)

(setq-default indicate-empty-lines t
              indicate-buffer-boundaries t)

;; MISC VARIABLES ;;

;; Whitespace mode
(setq whitespace-style 
      '(spaces tabs newline space-mark tab-mark newline-mark))
;; (setq whitespace-display-mappings
;;       `((space-mark 32 [183] [64])
;;         (newline-mark 10 [182 10])
;;         (tab-mark 9 [9655 9] [92 9])
;;         ))

;; IDO mode ;;
(require 'recentf)
(recentf-mode)

(ido-mode t)
(setq ido-create-new-buffer 'always
      ido-default-buffer-method 'selected-window
      ido-enable-last-directory-history nil
      ido-enable-flex-matching t)

;; Navigate windows with Shift-<arrows>
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(defun my:minibuffer-set-key (key command)
  ;; Binds key to all common minibuffer states
  (dolist (m (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map))
    (define-key m key command)))

(defun my:move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(my:minibuffer-set-key [escape] 'minibuffer-keyboard-quit)
(global-set-key (kbd "C-S-g") 'keyboard-escape-quit)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)


(global-set-key (kbd "M-/") 'hippie-expand)

(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Used for system keyboard layout switch
(global-set-key (kbd "M-SPC") nil)

;; Swap lines

(defun my:move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun my:move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Project root helpers 

(unless (boundp 'my:project-root)
  (setq my:project-root nil))
(defun my:project-root-set ()
  ;; Sets global project root to directory
  (interactive)
  (setq my:project-root 
        (expand-file-name (ido-read-directory-name "Set project dir: "))))
(defun my:project-root-unset ()
  ;; Resets global project root
  (interactive)
  (setq my:project-root nil))

;; Buffer management
(defadvice quit-window (before quit-window-kill-buffer activate)
  (ad-set-arg 0 t))

(defun my:kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer))) 

(defun my:kill-and-close-current ()
  (interactive)
  (let ((buf (current-buffer)))
    (when (not (one-window-p))
      (delete-window))
    (kill-buffer buf)))

(global-set-key (kbd "C-x C-k") 'my:kill-and-close-current)

;; ### PACKAGES ### ;;
(defun my:package-initialize ()
  ;; Navigation, editing, appearance
  (use-package smooth-scrolling
    :ensure t
    :config (setq smooth-scroll-margin 4))
  (use-package linum-relative
    :ensure t
    :config (progn (setq-default linum-relative-format "%4s ")))
  (use-package undo-tree
    :ensure t
    :config (global-undo-tree-mode))
  (use-package smart-mode-line
    :ensure t
    :pre-init (progn (setq sml/theme 'dark))
    :config (progn (sml/setup)))
  (use-package buffer-move
    :ensure t
    :config (progn
              (global-set-key (kbd "<C-S-up>") 'buf-move-up)
              (global-set-key (kbd "<C-S-down>") 'buf-move-down)
              (global-set-key (kbd "<C-S-left>") 'buf-move-left)
              (global-set-key (kbd "<C-S-right>") 'buf-move-right)))
  (use-package multiple-cursors
    :ensure t
    :config (progn
              (global-set-key (kbd "C->") 'mc/mark-next-like-this)
              (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
              (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))
  (use-package expand-region
    :ensure t
    :config (progn
              (global-set-key (kbd "C-t") 'er/expand-region)
              (global-set-key (kbd "C-S-t") 'er/contract-region)))
  (use-package smartparens
    :ensure t
    :config (progn
              (setq sp-show-pair-from-inside t)
              (smartparens-global-mode t)
              (show-smartparens-global-mode t)))
  (use-package ace-jump-mode
    :ensure t)
  ;; Completion
  (use-package auto-complete-config
    :ensure auto-complete
    :config (progn
              (setq ac-use-menu-map t)
              (define-key ac-menu-map (kbd "C-p") 'ac-previous)
              (define-key ac-menu-map (kbd "C-n") 'ac-next)
              (ac-config-default)
              (ac-linum-workaround)))
  (use-package yasnippet
    :ensure t
    :idle (yas-global-mode t)
    :config (progn 
              (add-to-list 'yas-snippet-dirs my:snippets-dir)
              (define-key yas-minor-mode-map (kbd "<tab>") nil)
              (define-key yas-minor-mode-map (kbd "TAB") nil)
              (add-to-list 'hippie-expand-try-functions-list
                           'yas-hippie-try-expand)))
  ;; Fast access and searching
  (use-package flx-ido
    :ensure t
    :config (progn
              (flx-ido-mode 1)
              (setq ido-use-faces nil)))
  (use-package ido-ubiquitous
    :ensure t
    :config (ido-ubiquitous))
  (use-package ido-vertical-mode
    :ensure t
    :config (ido-vertical-mode 1))
  (use-package smex
    :ensure t
    :config (progn 
              (global-set-key (kbd "M-x") 'smex)
              (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
  (use-package ag
    :ensure t
    :config (progn (setq ag-highlight-search t)))
  ;; Project management and project tree
  (use-package projectile
    :ensure t
    :config (progn
              (defadvice projectile-project-root (around projectile-my-root activate)
                (if (and my:project-root
                         (s-starts-with? my:project-root
                                         default-directory))
                    (setq ad-return-value my:project-root)
                  ad-do-it))
              (projectile-global-mode)))
  (use-package project-explorer
    :ensure t
    :config (progn
              (global-set-key (kbd "<f5>") 'project-explorer-open)))
  (use-package sr-speedbar
    :ensure t
    :config (progn
              (setq speedbar-use-images nil)
              (global-set-key (kbd "<f7>") 'sr-speedbar-toggle)))
  ;; Evil mode and Co
  (use-package evil
    :ensure t
    :pre-load (progn
                (setq evil-want-C-u-scroll t
                      evil-want-C-w-in-emacs-state t))
    :config (progn
              (setq evil-emacs-state-modes '(Custom-mode
                                             direx:direx-mode
                                             project-explorer-mode
                                             cider-repl-mode))
              (define-key evil-insert-state-map (kbd "C-SPC") 'auto-complete)
              (define-key evil-insert-state-map (kbd "C-@") 'auto-complete)
              (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)
              (evil-define-state normal-im
                "Motion with input method for searching"
                :tag " <S> "
                :enable (motion)
                :input-method t)
              (defadvice evil-search-incrementally (around evil-search-f-method activate)
                (evil-normal-im-state) ad-do-it (evil-normal-state)) 
              (setq evil-search-module 'evil-search
                    evil-want-C-u-scroll t
                    evil-want-C-w-in-emacs-state t)
              (evil-mode 1)))
  (use-package evil-leader
    :ensure t
    :config (progn
              (global-evil-leader-mode)))
  ;; Language specific
  (use-package jedi
    :ensure t)
  (use-package js2-mode
    :ensure t
    :config (progn (add-to-list 'auto-mode-alist '("\\.js" . js2-mode))))
  (use-package clojure-mode
    :ensure t)
  (use-package cider
    :ensure t)
  (use-package ac-nrepl
    :ensure t
    :config (progn
              (add-hook 'cider-mode-hook 'ac-nrepl-setup))))


(package-initialize)
(when (require 'use-package nil t)
  (my:package-initialize))



