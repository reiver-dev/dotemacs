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

(require 'package)
(add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; python ;;
(defconst my:python-path
  (concat (getenv "PYTHONPATH") ":" (concat my:user-dir "python-path")))
(setenv "PYTHONPATH" my:python-path)

;; backup and autosave ;;
(setq
 backup-directory-alist `((".*" . ,my:backup-dir))
 auto-save-list-file-prefix my:autosave-dir
 auto-save-file-name-transforms `((".*" ,my:autosave-dir t)))

;; APPEARANCE ;;

;; toolbars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line-highlight nil
                    :box nil
                    :inverse-video t)

(blink-cursor-mode -1)

(setq-default inhibit-startup-screen t
              initial-scratch-message nil
              color-theme-is-global t
              visible-bell t)

;; Line numbers and fringe
(require 'linum)
(add-hook 'prog-mode-hook 'linum-mode)

(setq-default indicate-empty-lines t
              indicate-buffer-boundaries t)

;; Scroll
(setq-default mouse-wheel-scroll-amount '(3 ((shift) . 1)) ;; three line at a time
              mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
              mouse-wheel-follow-mouse t) ;; scroll window under mouse

;; Text behavior
(setq-default shift-select-mode nil
              truncate-lines nil
              word-wrap t
              sentence-end-double-space nil)

;; Indentation ;;
(electric-indent-mode t)
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 120)


;; Spell Check ;;
(when (executable-find "hunspell")
  (require 'ispell)
  (add-to-list 'ispell-local-dictionary-alist
               '("russian-hunspell" "[Ё-ё]" "[^Ё-ё]" "[-]" nil ("-d" "ru_RU") nil utf-8))
  (add-to-list 'ispell-local-dictionary-alist
               '("english-hunspell" "[A-z]" "[^A-z]" "[']" nil ("-d" "en_GB") nil iso-8859-1))
  (setq ispell-program-name "hunspell"))

(setq flyspell-issue-message-flag nil)

;; IDO mode ;;
(ido-mode t)
(recentf-mode t)

(setq-default ido-create-new-buffer 'always
              ido-default-buffer-method 'selected-window
              ido-enable-last-directory-history nil
              ido-enable-flex-matching t)
(add-to-list 'ido-ignore-buffers "^\\*helm")

;; Navigate windows with Shift-<arrows> ;;
(windmove-default-keybindings)
(setq-default windmove-wrap-around t)
(winner-mode 1)

;; Comint ;;
(setq-default comint-prompt-read-only t
              comint-scroll-to-bottom-on-input t)

;; Util ;;
(setq my:bindings-mode-map (make-sparse-keymap))

(define-minor-mode my:bindings-mode
  "My settings"
  :global t
  :keymap my:bindings-mode-map)

(defun my:kmap (key function)
  (define-key my:bindings-mode-map (kbd (concat "C-; " key)) function)
  (define-key my:bindings-mode-map (kbd (concat "C-c ; " key)) function))

(my:bindings-mode 1)

(defun my:minibuffer-set-key (key command)
  "Binds key to all common minibuffer states"
  (dolist (m (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map))
    (define-key m key command)))

(defun my:move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

;; Interactive ;;

(defun my:push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun my:jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defadvice exchange-point-and-mark
  (before exchange-pnm-no-activate activate compile)
  (ad-set-arg 0 t))

(defun my:kill-line-to-indent ()
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

;; Swap lines
(defun my:move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (previous-line 2)
  (indent-according-to-mode))

(defun my:move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (previous-line 1)
  (transpose-lines 1)
  (indent-according-to-mode))

;; Project root helpers
(defvar my:project-root nil)
(defun my:project-root-set ()
  "Sets global project root to directory"
  (interactive)
  (setq my:project-root
        (expand-file-name (ido-read-directory-name "Set project dir: "))))
(defun my:project-root-unset ()
  "Resets global project root"
  (interactive)
  (setq my:project-root nil))

;; Buffer management
(defadvice quit-window (before quit-window-kill-buffer activate)
  (ad-set-arg 0 t))

(defun my:minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my:kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun my:kill-and-close-current ()
  (interactive)
  (let ((buf (current-buffer)))
    (when (not (one-window-p))
      (delete-window))
    (kill-buffer buf)))

;; Bindings ;;
(global-unset-key (kbd "ESC ESC ESC"))
(global-set-key (kbd "C-S-g") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-g") 'keyboard-escape-quit)
(global-set-key (kbd "<escape>") 'keyboard-quit)
(my:minibuffer-set-key (kbd "<escape>") 'my:minibuffer-keyboard-quit)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-c") 'switch-to-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "C-c o") 'ff-find-other-file)

(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t u") 'my:move-line-up)
(global-set-key (kbd "M-t d") 'my:move-line-down)
(global-set-key (kbd "M-t s") 'transpose-sexps)

(global-set-key (kbd "C-x m") 'my:push-mark-no-activate)
(global-set-key (kbd "C-x p") 'pop-to-mark-command)

(global-set-key (kbd "C-x C-k") 'my:kill-and-close-current)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)

(global-set-key (kbd "C-<backspace>") 'my:kill-line-to-indent)
(global-set-key (kbd "C-<delete>") 'kill-line)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-<delete>") 'kill-word)


;; ### PACKAGES ### ;;
(defun my:package-initialize ()
  ;; Navigation, editing, appearance
  (use-package linum-relative
    :ensure t
    :init (progn (setq-default linum-relative-format "%4s ")))
  (use-package undo-tree
    :ensure t
    :config (progn
            (global-undo-tree-mode)
            (setq undo-tree-visualizer-timestamps t
                  undo-tree-visualizer-diff t)))
  (use-package powerline
    :ensure t
    :config (powerline-center-evil-theme))
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
              (global-set-key (kbd "C-=") 'er/expand-region)))
  (use-package smartparens
    :ensure t
    :config (progn
              (setq sp-show-pair-from-inside t)
              (smartparens-global-mode t)
              (show-smartparens-global-mode t)))
  (use-package ace-jump-mode
    :ensure t)
  ;; Completion
  (use-package company
    :ensure t
    :config (progn
              (define-key company-mode-map (kbd "C-<tab>") 'company-complete)
              (define-key company-active-map (kbd "C-n") 'company-select-next)
              (define-key company-active-map (kbd "C-p") 'company-select-previous)
              (global-company-mode)))
  (use-package yasnippet
    :ensure t
    :config (progn
              (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt yas-no-prompt))
              (add-to-list 'yas-snippet-dirs my:snippets-dir)
              (defadvice yas-expand (before advice-for-yas-expand activate)
                (sp-remove-active-pair-overlay))
              (yas-global-mode t)))
  ;; Fast access and searching
  (use-package helm-config
    :ensure helm
    :config (progn
              (helm-mode 1)
              (defun my:helm-completion (engine actions)
                (mapc
                 (lambda (action)
                   (add-to-list 'helm-completing-read-handlers-alist `(,action . ,engine)))
                 actions))
              (my:helm-completion nil '(switch-to-buffer kill-buffer))
              (set-face-attribute 'helm-selection nil :underline nil)
              (set-face-attribute 'helm-selection-line nil :underline nil)
              (set-face-attribute 'helm-ff-directory nil :background nil)
              (my:kmap "t" 'helm-etags-select)
              (my:kmap "i" 'helm-semantic-or-imenu)
              (my:kmap "e" 'helm-list-emacs-process)
              (my:kmap "r" 'helm-resume)
              (global-set-key (kbd "C-x b") 'helm-mini)
              (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
              (global-set-key (kbd "C-x C-f") 'helm-find-files)
              (global-set-key (kbd "M-x") 'helm-M-x)
              (global-set-key (kbd "M-y") 'helm-show-kill-ring)
              (global-set-key (kbd "M-y") 'helm-show-kill-ring)
              (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
              (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
              (define-key helm-map (kbd "C-z") 'helm-select-action)))
  (use-package helm-swoop
    :ensure t)
  (use-package ido-vertical-mode
    :ensure t
    :config (ido-vertical-mode 1))
  (use-package flx-ido
    :ensure t
    :config (progn
              (flx-ido-mode 1)
              (setq ido-use-faces nil)))
  (use-package ido-ubiquitous
    :ensure t
    :disabled t
    :config (ido-ubiquitous))
  (use-package smex
    :ensure t
    :disabled t
    :config (progn
              (global-set-key (kbd "M-x") 'smex)
              (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
  (use-package ag
    :ensure t
    :config (progn (setq ag-highlight-search t)))
  ;; External tools
  (use-package magit
    :defer t
    :ensure t)
  ;; Project management and project tree
  (use-package neotree
    :ensure t
    :init (progn
            (setq neo-persist-show nil)
            (global-set-key (kbd "<f5>") 'neotree-toggle)
            (global-set-key (kbd "<f6>") 'neotree-find)
            (define-key neotree-mode-map (kbd "r") 'neotree-refresh)
            (define-key neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)
            (define-key neotree-mode-map (kbd "a") 'neotree-stretch-toggle)
            (define-key neotree-mode-map (kbd "p") 'neotree-previous-node)
            (define-key neotree-mode-map (kbd "n") 'neotree-next-node)
            (define-key neotree-mode-map (kbd "k") 'neotree-previous-node)
            (define-key neotree-mode-map (kbd "j") 'neotree-next-node)
            (defadvice neotree-enter (after my:nt-jump-to-button activate)
              (unless (button-at (point))
                (neotree-next-node)))))
  (use-package projectile
    :ensure t
    :config (progn
              (defun my:neotree-project-root ()
                "Jump neotree to current project root (if exists)"
                (interactive)
                (let ((root (projectile-project-root)))
                      (neotree-dir root)))
              (global-set-key (kbd "<f7>") 'my:neotree-project-root)
              (defadvice projectile-project-root (around projectile-my-root activate)
                (if (and my:project-root
                         (s-starts-with? my:project-root
                                         default-directory))
                    (setq ad-return-value my:project-root)
                  ad-do-it))
              (projectile-global-mode)))
  (use-package helm-projectile
    :ensure t
    :config (progn
              (my:kmap "p" 'helm-projectile)))
  ;; Evil mode and Co
  (use-package evil
    :ensure t
    :pre-load (progn
                (setq evil-want-C-u-scroll t
                      evil-want-C-w-in-emacs-state t
                      evil-want-visual-char-semi-exclusive t)
                (global-set-key (kbd "C-S-w") 'kill-region))
    :config (progn
              (setq evil-default-state 'emacs)
              (mapc (lambda (mode)
                      (evil-set-initial-state mode 'normal))
                    '(nxml-mode))
              (evil-set-initial-state 'neotree-mode 'motion)
              (add-hook 'prog-mode-hook 'evil-normal-state)
              (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)
              (add-hook 'neotree-mode-hook
                        (lambda ()
                          (define-key evil-motion-state-local-map (kbd "TAB") 'neotree-enter)
                          (define-key evil-motion-state-local-map (kbd "SPC") 'neotree-enter)
                          (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter)
                          (define-key evil-motion-state-local-map (kbd "q") 'neotree-hide)))
              (evil-mode 1)))
  (use-package evil-leader
    :disabled t
    :ensure t
    :config (progn
              (global-evil-leader-mode)))
  ;; Language specific
  (use-package anaconda-mode
    :ensure t
    :init (add-hook 'python-mode-hook 'anaconda-mode))
  (use-package company-anaconda
    :ensure t
    :init (add-to-list 'company-backends 'company-anaconda))
  (use-package flycheck
    :ensure t)
  (use-package js2-mode
    :ensure t
    :init (progn (add-to-list 'auto-mode-alist '("\\.js" . js2-mode))))
  (use-package clojure-mode
    :ensure t)
  (use-package cider
    :ensure t))

(package-initialize)
(when (require 'use-package nil t)
  (my:package-initialize))
