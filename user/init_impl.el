;;  MAIN  ;;

;; my folders ;;
(defconst user-emacs-directory-full (expand-file-name user-emacs-directory))

(defconst my:user-dir (concat user-emacs-directory-full "user/"))
(defconst my:modules-dir (concat my:user-dir "modules/"))

(defconst my:backup-dir (concat user-emacs-directory-full "backup/"))
(defconst my:autosave-dir (concat user-emacs-directory-full "autosave/"))
(defconst my:recipes-dir (concat my:user-dir "el-get-recipes/"))
(defconst my:snippets-dir (concat my:user-dir "snippets/"))

(dolist (dir (list my:backup-dir my:autosave-dir my:modules-dir my:snippets-dir))
  (unless (file-directory-p dir)
    (make-directory dir)))

;; custom file ;;
(setq custom-file (concat user-emacs-directory-full "custom.el"))
(load custom-file t)

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

;; toolbars
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(set-default-font "Meslo LG S 10")
(show-paren-mode t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
;; (if (fboundp 'fringe-mode)
;;     (fringe-mode 0))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight the current line
;;(global-hl-line-mode +1)

;; scroll
(setq-default mouse-wheel-scroll-amount '(3 ((shift) . 1)) ;; three line at a time
              mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
              mouse-wheel-follow-mouse t) ;; scroll window under mouse

;; smart pairing for all
(electric-pair-mode t)
(delete-selection-mode t)

;; Easily navigate sillycased words
(global-subword-mode 1)
 
;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; tabs
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 120
              electric-indent-mode t)

(setq-default linum-format "%4d ")
(let ((frc (face-attribute 'fringe :background)))
  (set-face-attribute 'linum nil :background frc))
(global-linum-mode)

;; MISC VARIABLES ;;
(setq-default
 visible-bell t
 inhibit-startup-message t
 indicate-empty-lines nil
 color-theme-is-global t
 shift-select-mode t
 truncate-lines t)

;; Whitespace mode
(setq whitespace-style 
      '(spaces tabs newline space-mark tab-mark newline-mark))
;; (setq whitespace-display-mappings
;;       `((space-mark 32 [183] [64])
;;         (newline-mark 10 [182 10])
;;         (tab-mark 9 [9655 9] [92 9])
;;         ))

;; IDO mode ;;
(ido-mode t)
(setq ido-create-new-buffer 'always
      ido-default-buffer-method 'selected-window
      ido-case-fold t
      ido-enable-last-directory-history nil
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      ido-enable-flex-matching t
      ido-max-prospects 15
      ido-confirm-unique-completion t
      ido-decorations
      '("\n-> "
        ""
        "\n   "
        "\n ..."
        "["
        "]"
        " [No match]"
        " [Matched]"
        " [Not readable]"
        " [Too big]"
        " [Confirm]"))

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

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


;; ### PACKAGES ### ;;

(add-to-list 'load-path (concat user-emacs-directory-full "/el-get/el-get"))

(when (require 'el-get nil t)
  (setq el-get-verbose t)
  (add-to-list 'el-get-recipe-path my:recipes-dir)
  (setq el-get-sources
        '((:name el-get
                 :branch "master")
          smooth-scrolling
          (:name undo-tree
                 :before (progn
                           (setq undo-tree-mode-lighter ""))
                 :after (progn
                          (global-undo-tree-mode)))
          (:name buffer-move
                 :after (progn
                          (global-set-key (kbd "<M-S-up>") 'buf-move-up)
                          (global-set-key (kbd "<M-S-down>") 'buf-move-down)
                          (global-set-key (kbd "<M-S-left>") 'buf-move-left)
                          (global-set-key (kbd "<M-S-right>") 'buf-move-right)))
          (:name multiple-cursors
                 :after (progn
                          (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                          (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                          (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))
          (:name expand-region
                 :after (progn
                          (global-set-key (kbd "C-t") 'er/expand-region)
                          (global-set-key (kbd "C-S-t") 'er/contract-region)))
          (:name jump-char
                 :before (progn
                           (global-set-key (kbd "C-c C-f") 'jump-char-forward)
                           (global-set-key (kbd "C-c f") 'jump-char-backward)))
          ;ido-ubiquitous
          ;; (:name smart-mode-line
          ;;        :after (progn
          ;;                 (sml/setup)))
          (:name smex
                 :after (progn ()
                               (global-set-key (kbd "M-x") 'smex)
                               (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
          (:name yasnippet
                 :after (progn ()
                               (add-to-list 'yas-snippet-dirs my:snippets-dir)
                               (yas-global-mode t)))
          (:name helm
                 :before (progn
                           (global-set-key (kbd "C-c h") 'helm-mini)))
          (:name auto-complete
                 :features auto-complete-config
                 :after (progn
                          (defun ac-common-setup ()
                            (add-to-list 'ac-sources 'ac-source-yasnippet t))
                          (ac-config-default)
                          (ac-linum-workaround)))
          project-explorer
          projectile
          (:name ag
                 :after (progn
                          (setq ag-highlight-search t)))
          (:name hlinum
                 :after (progn
                          (set-face-attribute 
                           'linum-highlight-face nil 
                           :foreground (face-attribute 'linum :foreground)
                           :background (face-attribute 'hl-line-face :background))
                          (hlinum-activate)))
          js2-mode))
  (setq my:packages 
         (mapcar 'el-get-as-symbol 
                 (mapcar 'el-get-source-name el-get-sources)))
  (defun my:sync-packages ()
    (interactive)
    (el-get my:packages))
  (defun my:clean-packages ()
    (interactive)
    (el-get-cleanup my:packages))
  (el-get 'sync))
