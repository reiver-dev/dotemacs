;;  MAIN  ;;

;; my folders ;;
(defconst user-emacs-directory-full (expand-file-name user-emacs-directory))

(defconst my:user-dir (concat user-emacs-directory-full "user/"))
(defconst my:modules-dir (concat my:user-dir "modules/"))

(defconst my:backup-dir (concat user-emacs-directory-full "backup/"))
(defconst my:autosave-dir (concat user-emacs-directory-full "autosave/"))
(defconst my:recipes-dir (concat my:user-dir "el-get-recipes/"))
(defconst my:el-get-dir (concat user-emacs-directory-full "/el-get/el-get/"))
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

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(blink-cursor-mode -1)

(setq-default visible-bell t
              inhibit-startup-message t
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

(add-to-list 'load-path my:el-get-dir)

(defun my:packages-el-get ()
  (interactive)
  (let ((url "https://github.com/dimitri/el-get"))
    (start-process "el-get-download"
                   (get-buffer-create "*el-get-download*")
                   "git" "clone" url my:el-get-dir)))

(when (require 'el-get nil t)
  (setq el-get-verbose t)
  (add-to-list 'el-get-recipe-path my:recipes-dir)
  (setq el-get-sources
        '((:name el-get
                 :branch "master")
          ;; Navigation, editing, appearance
          (:name smooth-scrolling)
          (:name linum-relative
                 :after (progn (setq-default linum-relative-format "%4s ")))
          (:name undo-tree
                 :before (progn
                           (setq undo-tree-mode-lighter ""))
                 :after (progn
                          (global-undo-tree-mode)))
          (:name smart-mode-line
                 :before (progn (setq sml/theme 'dark))
                 :after (progn (sml/setup)))
          (:name buffer-move
                 :after (progn
                          (global-set-key (kbd "<C-S-up>") 'buf-move-up)
                          (global-set-key (kbd "<C-S-down>") 'buf-move-down)
                          (global-set-key (kbd "<C-S-left>") 'buf-move-left)
                          (global-set-key (kbd "<C-S-right>") 'buf-move-right)))
          (:name multiple-cursors
                 :after (progn
                          (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                          (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                          (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))
          (:name expand-region
                 :after (progn
                          (global-set-key (kbd "C-t") 'er/expand-region)
                          (global-set-key (kbd "C-S-t") 'er/contract-region)))
          (:name smartparens
                 :after (progn
                          (setq sp-show-pair-from-inside t)
                          (smartparens-global-mode t)
                          (show-smartparens-global-mode t)))
          ;; Completion
          (:name auto-complete
                 :features auto-complete-config
                 :after (progn
                          (setq ac-use-menu-map t)
                          (define-key ac-menu-map (kbd "C-p") 'ac-previous)
                          (define-key ac-menu-map (kbd "C-n") 'ac-next)
                          (ac-config-default)
                          (ac-linum-workaround)))
          (:name yasnippet
                 :features yasnippet
                 :after (progn 
                          (add-to-list 'yas-snippet-dirs my:snippets-dir)
                          (define-key yas-minor-mode-map (kbd "<tab>") nil)
                          (define-key yas-minor-mode-map (kbd "TAB") nil)
                          (yas-global-mode t)
                          (add-to-list 'hippie-expand-try-functions-list
                                       'yas-hippie-try-expand)))
          ;; Fast access and searching
          (:name ido-vertical-mode
                 :after (ido-vertical-mode 1))
          (:name smex
                 :after (progn 
                          (global-set-key (kbd "M-x") 'smex)
                          (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
          (:name helm
                 :before (progn
                           (global-set-key (kbd "C-c h") 'helm-mini)
                           (helm-mode t)))
          (:name ag
                 :after (progn (setq ag-highlight-search t)))
          ;; Project management and project tree
          (:name projectile
                 :after (progn
                          (defadvice projectile-project-root (around projectile-my-root activate)
                            (if (and my:project-root
                                     (s-starts-with? my:project-root
                                                     default-directory))
                                (setq ad-return-value my:project-root)
                              ad-do-it))
                          (global-set-key (kbd "<f6>") 'helm-projectile)
                          (projectile-global-mode)))
          (:name project-explorer
                 :after (progn
                          (global-set-key (kbd "<f5>") 'project-explorer-open)))
          (:name sr-speedbar
                 :after (progn
                          (setq speedbar-use-images nil)
                          (global-set-key (kbd "<f7>") 'sr-speedbar-toggle)))
          ;; Evil mode and Co
          (:name evil
                 :after (progn
                          (setq evil-emacs-state-modes '(Custom-mode
                                                         direx:direx-mode
                                                         project-explorer-mode
                                                         cider-repl-mode))
                          (define-key evil-insert-state-map (kbd "C-SPC") 'auto-complete)
                          (evil-define-state normal-im
                            "Motion with input method for searching"
                            :tag " <S> "
                            :enable (motion)
                            :input-method t)
                          (defadvice evil-search-incrementally (around evil-search-f-method activate)
                            (evil-normal-im-state) ad-do-it (evil-normal-state)) 
                          (evil-mode 1)))
          (:name evil-leader
                 :after (progn
                          (global-evil-leader-mode)))
          ;; Language specific
          (:name js2-mode
                 :after (progn (add-to-list 'auto-mode-alist '("\\.js" . js2-mode))))
          (:name clojure-mode)
          (:name cider)
          (:name ac-nrepl
                 :after (progn
                          (add-hook 'cider-mode-hook 'ac-nrepl-setup)))))
  (setq my:packages 
        (mapcar 'el-get-as-symbol 
                (mapcar 'el-get-source-name el-get-sources)))
  (defun my:packages-sync () 
    (interactive)
    (el-get 'sync my:packages))
  (defun my:packages-async () 
    (interactive)
    (el-get nil my:packages))
  (defun my:packages-clean ()
    (interactive)
    (el-get-cleanup my:packages)))


(when (require 'el-get nil t)
  (el-get 'sync))
