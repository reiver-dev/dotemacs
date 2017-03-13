;;; init-main.el --- Main config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-keybind)
(require 'init-wm)
(require 'init-project)
(require 'init-edit)
(require 'init-environ)
(require 'init-spellcheck)

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("marmelade" . "https://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("org" . "http://orgmode.org/elpa/")))


(defun package--compile---no-safe (proc &rest args)
  "Ignore unsaved files during package install"
  (let ((old (symbol-function 'save-some-buffers)))
    (unwind-protect
        (progn (fset 'save-some-buffers 'ignore)
               (apply proc args))
      (fset 'save-some-buffers old))))


(advice-add 'package--compile :around
            #'package--compile---no-safe)


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
(fringe-mode '(nil . 0))

;; Whitespace
(setq-default whitespace-line-column 79
              whitespace-style
              '(face indentation space-before-tab trailing lines-tail))


(defun my:whitespace-mode-diff-setup ()
  (setq-local whitespace-style
              '(face tabs tab-mark spaces space-mark trailing
                     indentation::space indentation::tab
                     newline newline-mark)))


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

(if (eq (lookup-key (current-global-map) (kbd "M-*"))
        'pop-tag-mark)
    (my:kmap
     ("M-," #'pop-tag-mark)
     ("M-*" #'tags-loop-continue)))


;;; Mode Settings

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
              comint-input-ignoredups t
              comint-scroll-show-maximum-output t
              comint-scroll-to-bottom-on-input t
              comint-scroll-to-bottom-on-output nil
              comint-buffer-maximum-size 8196)

(defun my:-comint-text-readonly (text)
  (let ((inhibit-read-only t)
        (output-end (process-mark (get-buffer-process (current-buffer)))))
    (put-text-property comint-last-output-start output-end 'read-only t)))

(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-output-filter-functions 'my:-comint-text-readonly)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(with-eval-after-load 'comint
  ;; We have `my:kill-region-or-word' already
  (my:kmap* comint-mode-map ("C-c C-w" nil)))


;;; Compilation
(setq compilation-ask-about-save nil        ;; save everything
      compilation-scroll-output 'next-error ;; stop on error
      compilation-skip-threshold 2)         ;; skip warnings

(defun my:compile (comint)
  "Compile without confirmation.
With a prefix argument, use `comint-mode'."
  (interactive "P")
  ;; Do the command without a prompt.
  (save-window-excursion
    (compile (eval compile-command) (and comint t)))
  ;; Create a compile window of the desired width.
  (pop-to-buffer (get-buffer "*compilation*"))
  (enlarge-window
   (- (frame-width) 105 (window-width))
   'horizontal))

(with-eval-after-load 'compile
  (my:kmap* compilation-shell-minor-mode-map
            ("<f8>" "<C-<f8>" #'recompile)))

(my:kmap* prog-mode-map
          ("<f8>" #'my:compile)
          ("C-<f8>" #'compile))

;; Eshell
(setq-default eshell-scroll-to-bottom-on-input t)

(defun eshell/ff (&rest args)
 (dolist (f args)
   (find-file f t)))

(defun eshell/fo (&rest args)
  (dolist (f args)
    (find-file-other-window f t)))

(defun eshell/d (&rest args)
  (dired (pop dir) "."))

;; Disable everything for big files
(defun my:large-file-p ()
  (< large-file-warning-threshold (buffer-size)))

(define-derived-mode my:large-file-mode fundamental-mode "LargeFile"
  "Mode to minimize large file freezes"
  (setq bidi-display-reordering nil))

(add-to-list 'magic-mode-alist
             (cons #'my:large-file-p #'my:large-file-mode))


;; External tools
(defun my:process-region-with-command (command)
  (let ((begin (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region begin end command nil t)))


(defun my:reindent-xml ()
  (interactive)
  (my:process-region-with-command "xmllint --format -"))


(defun my:reindent-json ()
  (interactive)
  (my:process-region-with-command "python -m json.tool"))


(package-initialize)

(require 'init-pkgs)
(require 'init-parens)
(require 'init-completion)
(require 'init-cc)
(require 'init-python)
(require 'init-clojure)
(require 'init-org)
(require 'init-haskell)
(require 'init-spellcheck)


(provide 'init-main)

;;; init-main.el ends here
