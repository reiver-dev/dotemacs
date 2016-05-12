;;; init-main.el --- Main config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-keybind)
(require 'init-wm)
(require 'init-project)
(require 'init-edit)
(require 'init-spellcheck)

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("marmelade" . "https://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/")))

;;; Appearance
(require 'init-theme)
(enable-theme 'my:theme)

;; Toolbars
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'toolltip-mode)
  (tooltip-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Enable y/n answers
(fset #'yes-or-no-p #'y-or-n-p)
(setq use-dialog-box nil)

;; Modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Fringe
(setq-default indicate-empty-lines t)
(fringe-mode '(8 . 0))

;; Whitespace
(setq-default whitespace-line-column 79
              whitespace-style
              '(face trailing empty indentation
                     space-before-tab space-after-tab))


;; Misc settings
(defun my:bell-function ()
  (unless (memq this-command
                '(isearch-abort
                  abort-recursive-edit
                  exit-minibuffer
                  keyboard-quit
                  helm-keyboard-quit))
    (ding)))


(setq-default inhibit-startup-screen t
              initial-scratch-message nil
              visible-bell t ;; do not beep
              ring-bell-function #'my:bell-function ;; and blink less
              disabled-command-function nil) ;; enable all commands

;; Scroll
(setq-default
 ;; three lines at a time
 mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control)))
 ;; don't accelerate scrolling
 mouse-wheel-progressive-speed nil
 ;; don't recenter
 scroll-conservatively 100000
 scroll-margin 0
 scroll-preserve-screen-position t
 ;; horizontal scroll jumps
 hscroll-step 1
 hscroll-margin 5)

;; Text behavior
(setq-default shift-select-mode nil
              sentence-end-double-space nil
              require-final-newline t)

(setq-default
 truncate-lines t ;; do not wrap lines
 bidi-display-reordering nil) ;; disable bidirectional text

;; Indentation
(setq-default indent-tabs-mode nil)

(defun my:prog-mode-setup ()
  "Basic settings for prog and other modes"
  (setq show-trailing-whitespace t)
  (whitespace-mode t))


(setq-default dired-listing-switches "-lhvA"
              dired-clean-up-buffers-too t
              dired-recursive-copies 'always
              dired-recursive-deletes 'top
              dired-hide-details-hide-symlink-targets nil)


(add-hook 'prog-mode-hook #'my:prog-mode-setup)
(add-hook 'nxml-mode #'my:prog-mode-setup)

;;; Global bindings

;; Redefine esc
;; however esc will not break from hangs like C-g
(global-unset-key (kbd "ESC ESC ESC"))
(global-set-key (kbd "C-M-g")           #'keyboard-escape-quit)
(global-set-key (kbd "<escape>")        #'keyboard-quit)
(my:minibuffer-set-key (kbd "<escape>") #'my:minibuffer-keyboard-quit)

;; Never quit so fast
(global-unset-key (kbd "C-x C-c"))
;; We have suspend at `C-x C-z'
(global-unset-key (kbd "C-z"))
;; Use that for instant bindings
(global-unset-key (kbd "<menu>"))

(mapc #'my:global-unset-command
      '(upcase-region downcase-region capitalize-region))

(my:kmap
 ([remap dabbrev-expand] #'hippie-expand) ; "M-/"
 ([remap dabbrev-completion] #'my:hippie-expand-files) ; "C-M-/"

 ;; To always keep M-x available
 ("M-X" #'execute-extended-command)

 ;; Jumping
 ([remap exchange-point-and-mark] #'my:exchange-point-and-mark) ; "C-x C-x"
 ("C-x m" #'my:push-mark-no-activate)
 ("C-c o" #'ff-find-other-file)
 ("C-; i" #'imenu)

 ;; Vim's word jumping
 ("M-a" (lambda (ARG)
          (interactive "^p")
          (forward-same-syntax (- ARG))))
 ("M-e" #'forward-same-syntax)

 ;; Buffers
 ([remap list-buffers] #'ibuffer) ; "C-x C-b"
 ("C-x k" #'my:kill-buffer)
 ("C-x K" #'my:kill-buffer-and-window)
 ("C-x C-c" #'switch-to-buffer)

 ;; Editing
 ("C-w" #'my:kill-region-or-word)
 ("C-S-w" #'kill-region)
 ("C-x C-;" #'comment-or-uncomment-region)

 ("C-h" #'delete-backward-char)
 ("M-h" #'my:backward-delete-word)

 ("M-<backspace>"
  [remap backward-kill-word] #'my:backward-delete-word)
 ("M-<delete>"
  [remap kill-word] #'my:delete-word)

 ("C-<backspace>" #'my:kill-line-to-indent)
 ("C-<delete>"    #'kill-line)

 ("M-k" #'kill-whole-line)
 ("M-j" #'my:join-line)

 ([remap capitalize-word] #'capitalize-dwim)
 ([remap upcase-word] #'upcase-dwim)
 ([remap downcase-word] #'downcase-dwim)

 ;; Window management
 ("C-c w <left>"  #'windmove-left)
 ("C-c w <down>"  #'windmove-down)
 ("C-c w <up>"    #'windmove-up)
 ("C-c w <right>" #'windmove-right)

 ("C-c w h" #'windmove-left)
 ("C-c w j" #'windmove-down)
 ("C-c w k" #'windmove-up)
 ("C-c w l" #'windmove-right)

 ("C-c w r" #'my:resize-window)
 ("C-c w n" #'my:detach-window)

 ("<f9>" #'my:toggle-window-dedicated)
 ("<f8>" #'compile))

(if (eq (lookup-key (current-global-map) (kbd "M-*"))
        'pop-tag-mark)
    (my:kmap
     ("M-," #'pop-tag-mark)
     ("M-*" #'tags-loop-continue)))


;;; Mode Settings

;; hippie settings from Prelude
(setq-default hippie-expand-try-functions-list
              '(try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-file-name-partially
                try-complete-file-name
                try-expand-all-abbrevs
                try-expand-list
                try-expand-line
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol))

;; Ediff
(with-eval-after-load 'ediff
  (setq-default
   ;; run control panel in same frame
   ediff-window-setup-function #'ediff-setup-windows-plain))

;; Sync unchanged buffers with filesystem
(global-auto-revert-mode t)

;; Show recent files
(recentf-mode t)

;; Window management
(setq-default windmove-wrap-around t)
(winner-mode t)

;; Comint
(setq-default comint-prompt-read-only t
              comint-process-echoes t
              comint-scroll-to-bottom-on-input t)

(with-eval-after-load 'comint
  ;; We have `my:kill-region-or-word' already
  (my:kmap* comint-mode-map ("C-c C-w" nil)))


(defun my:large-file? ()
  (< large-file-warning-threshold (buffer-size)))

(define-derived-mode my:large-file-mode fundamental-mode "LargeFile"
  "Mode to minimize large file freezes"
  (setq bidi-display-reordering nil))

(add-to-list 'magic-mode-alist
             (cons #'my:large-file? #'my:large-file-mode))

;; C/C++
(defconst my:c-style
  '("linux"
    (c-basic-offset . 4)
    (c-offsets-alist
     (innamespace . 0)
     (inline-open . 0))))

(with-eval-after-load 'cc-mode
  (c-add-style "reiver" my:c-style)
  (setq-default c-default-style
                '((c-mode . "reiver")
                  (c++-mode . "reiver")
                  (java-mode . "java")
                  (awk-mode . "awk")
                  (other . "gnu"))))


(package-initialize)
(require 'init-pkgs)
(require 'init-org)

(provide 'init-main)

;;; init-main.el ends here
