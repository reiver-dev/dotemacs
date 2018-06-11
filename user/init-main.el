;;; init-main.el --- Main config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvaralias 'my:first-frame-hook 'init:first-frame-hook)

(require 'init-font)
(require 'init-defs)
(require 'init-package)
(require 'init-keybind)
(require 'init-wm)
(require 'init-filesystem)
(require 'init-edit)
(require 'init-environ)

(defconst -my:gc-threshold 800000)

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
(minibuffer-depth-indicate-mode t)
(setq-default enable-recursive-minibuffers t)

;; Fringe
(setq-default indicate-empty-lines t
              fringes-outside-margins t)

;; Window border
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode +1)

;; Whitespace
(eval-when-compile
  (require 'whitespace))

(setq-default whitespace-line-column 79
              whitespace-style
              '(face indentation space-before-tab trailing lines-tail))


(defun my:whitespace-mode-diff-setup ()
  (setq-local whitespace-style
              '(face tabs tab-mark spaces space-mark trailing
                     indentation::space indentation::tab
                     newline newline-mark)))


;; Misc settings


(defface -my:bell-modeline-face '((t (:inherit error :inverse-video t)))
  "Face to be used for blining modeline as visual-bell"
  :group 'my:faces)


(defvar -my:bell-active-p nil
  "Flag used as mutex for `ring-bell-function' calls.")


(defvar my:bell-modeline-face '-my:bell-modeline-face)


(defun -my:bell-modeline-face-remap ()
  (let ((face-remapping-alist-backup
         (copy-alist face-remapping-alist)))
    (setq -my:bell-active-p t)
    (setq face-remapping-alist
          (append
           (delete (assq 'mode-line face-remapping-alist)
                   face-remapping-alist)
           (list
            (cons 'mode-line my:bell-modeline-face))))
    (force-mode-line-update)
    face-remapping-alist-backup))


(defun -my:bell-modeline-face-remap-restore (face-remap buffer)
  (with-current-buffer buffer
    (setq face-remapping-alist face-remap)
    (setq -my:bell-active-p nil)
    (force-mode-line-update)))


(defun -my:bell-impl ()
  "Blink modeline."
  (unless -my:bell-active-p
    (run-with-timer
     0.15 nil
     #'-my:bell-modeline-face-remap-restore
     (-my:bell-modeline-face-remap)
     (current-buffer))))


(defun my:bell-function ()
  (unless (memq this-command
                '(isearch-abort
                  abort-recursive-edit
                  exit-minibuffer
                  minibuffer-keyboard-quit
                  keyboard-quit
                  helm-keyboard-quit))
    (-my:bell-impl)))


(setq-default inhibit-startup-screen t
              initial-scratch-message nil
              visible-bell t ;; do not beep
              ring-bell-function #'my:bell-function ;; and blink less
              disabled-command-function nil) ;; enable all commands

(when (string-equal system-type "windows-nt")
  (setq inhibit-compacting-font-caches t
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)))

(defun -my:gc-disable ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun -my:gc-enable ()
  (setq gc-cons-threshold -my:gc-threshold))

(add-hook 'minibuffer-setup-hook #'-my:gc-disable)
(add-hook 'minibuffer-exit-hook #'-my:gc-enable)

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
              set-mark-command-repeat-pop t
              sentence-end-double-space nil
              require-final-newline t)

(show-paren-mode t)
(electric-pair-mode t)

(setq-default
 truncate-lines t ;; do not wrap lines
 bidi-display-reordering nil) ;; disable bidirectional text

;; Indentation
(setq-default indent-tabs-mode nil)

(defun my:prog-mode-setup ()
  "Basic settings for prog and other modes"
  (setq show-trailing-whitespace t)
  (whitespace-mode t))

(add-hook 'diff-mode-hook #'my:whitespace-mode-diff-setup)

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
;; Free face modification group as it is never used
(global-unset-key (kbd "M-o"))

(mapc #'my:global-unset-command
      '(upcase-region downcase-region capitalize-region))

(my:kmap
 ;; To always keep M-x available
 ("M-X" #'execute-extended-command)

 ;; Jumping
 ([remap exchange-point-and-mark] #'my:exchange-point-and-mark) ; "C-x C-x"
 ("C-x m" #'my:push-mark-no-activate)
 ("C-; o" #'ff-find-other-file)
 ("C-; i" #'imenu)

 ;; Vim's word jumping
 ("M-a" #'my:backward-same-syntax)
 ("M-e" #'my:forward-same-syntax)

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
 ([remap open-line] #'my:open-line)
 ("M-o" #'my:open-line-back)

 ([remap capitalize-word] #'capitalize-dwim)
 ([remap upcase-word] #'upcase-dwim)
 ([remap downcase-word] #'downcase-dwim)

 ;; Window management
 ("C-c w h" "C-c w <left>" #'windmove-left)
 ("C-c w j" "C-c w <down>" #'windmove-down)
 ("C-c w k" "C-c w <up>" #'windmove-up)
 ("C-c w l" "C-c w <right>" #'windmove-right)

 ("C-c w r" #'my:resize-window)
 ("C-c w n" #'my:detach-window)

 ("<f9>" #'my:toggle-window-dedicated)
 ("<f5>" #'revert-buffer))


;;; Mode Settings


(eval-when-compile
  (require 'autorevert))

(declare-function auto-revert-set-timer "autorevert")
;; Sync unchanged buffers with filesystem
(global-auto-revert-mode t)
;; Disable `Reverting buffer' messages
(setq-default auto-revert-verbose nil)

(defun my:log-tail-handler ()
  (setq-local auto-revert-interval 1)
  (auto-revert-set-timer)
  (read-only-mode t)
  (font-lock-mode 0)
  (goto-char (point-max)))

(add-hook 'auto-revert-tail-mode-hook #'my:log-tail-handler)

;; Show recent files
(my:with-package recentf
  :defer 1
  :init (recentf-mode t))

;; Window management
(setq-default windmove-wrap-around t)
(winner-mode t)


;; Disable everything for big files
(defun my:large-file-p ()
  "Check if current buffer is considered large.
See `large-file-warning-threshold'."
  (< large-file-warning-threshold (buffer-size)))

(define-derived-mode my:large-file-mode fundamental-mode "LargeFile"
  "Mode to minimize large file freezes"
  (setq bidi-display-reordering nil))

(add-to-list 'magic-mode-alist
             (cons #'my:large-file-p #'my:large-file-mode))


(setq shell-command-default-error-buffer "*Shell Command STDERR*")

;; External tools
(defun my:process-region-with-command (command)
  "Execute COMMAND string over active region or entire buffer."
  (let (begin end noncont)
    (if (region-active-p)
        (setq begin (region-beginning)
              end (region-end)
              noncont (region-noncontiguous-p))
      (setq begin (point-min)
            end (point-max)
            noncont nil))
    (when (< begin end)
      (shell-command-on-region
       begin end command
       ;; no buffer set, replace in current
       nil t
       ;; display error-bufffer
       shell-command-default-error-buffer t
       noncont))))


(defun my:reindent-xml ()
  "Use xmllint to format XML."
  (interactive)
  (my:process-region-with-command "xmllint --format -"))


(defun my:reindent-html ()
  "Use xmllint to format HTML."
  (interactive)
  (my:process-region-with-command "xmllint --format --html -"))


(defun my:reindent-json ()
  "Use python to format json."
  (interactive)
  (my:process-region-with-command "python -m json.tool"))


(require 'init-dired)
(require 'init-compile)
(require 'init-comint)
(require 'init-eshell)
(require 'init-compare)

(require 'init-pkgs)
(require 'init-parens)
(require 'init-completion)
(require 'init-cc)
(require 'init-python)
(require 'init-clojure)
(require 'init-rust)
(require 'init-tex)
(require 'init-org)
(require 'init-haskell)
(require 'init-spellcheck)
(require 'init-http)


(provide 'init-main)

;;; init-main.el ends here
