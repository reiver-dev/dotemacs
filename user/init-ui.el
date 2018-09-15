;;; init-ui.el --- UI configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-defs)
(require 'init-package)
(require 'init-theme)

(enable-theme 'my:theme)


(defun my:whitespace-mode-diff-setup ()
  "Enable additional whitespace visualization."
  (setq-local whitespace-style
              '(face tabs tab-mark spaces space-mark trailing
                     indentation::space indentation::tab
                     newline newline-mark)))

(add-hook 'diff-mode-hook #'my:whitespace-mode-diff-setup)


(defun my:prog-mode-setup ()
  "Basic settings for prog and other modes."
  (unless (my:exporting-p)
    (setq show-trailing-whitespace t
          word-wrap nil)
    (whitespace-mode t)))

(add-hook 'prog-mode-hook #'my:prog-mode-setup)
(add-hook 'nxml-mode-hook #'my:prog-mode-setup)


(defface -my:bell-modeline-face '((t (:inherit error :inverse-video t)))
  "Face to be used for blining modeline as visual-bell"
  :group 'my:faces)


(defvar -my:bell-active-p nil
  "Flag used as mutex for `ring-bell-function' calls.")


(defvar my:bell-modeline-face '-my:bell-modeline-face
  "Modeline face used as visual-bell.")


(defun -my:bell-modeline-face-remap ()
  "Paint modeline with `my:bell-modeline-face'."
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
  "Restore original modeline faces.
Set value of `face-remapping-alist' to FACE-REMAP for BUFFER."
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
  "Blink modeline as visual bell."
  (unless (memq this-command
                '(isearch-abort
                  abort-recursive-edit
                  exit-minibuffer
                  minibuffer-keyboard-quit
                  keyboard-quit
                  helm-keyboard-quit))
    (-my:bell-impl)))


(setq-default
 use-dialog-box nil

 inhibit-startup-screen t
 initial-scratch-message nil

 indicate-empty-lines t
 fringes-outside-margins t
 ;;; Scroll
 ;; three lines at a time
 mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control)))
 ;; don't accelerate scrolling
 mouse-wheel-progressive-speed nil
 ;; don't recenter
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 ;; horizontal scroll jumps
 hscroll-step 1
 hscroll-margin 5

 whitespace-line-column 79
 whitespace-style '(face indentation space-before-tab trailing lines-tail)

 window-divider-default-places 'right-only
 window-divider-default-bottom-width 0
 window-divider-default-right-width 1

 visible-bell t
 ring-bell-function #'my:bell-function
 truncate-lines t
 word-wrap t

 enable-recursive-minibuffers t
 minibuffer-prompt-properties '(read-only
                                t point-entered
                                minibuffer-avoid-prompt
                                face minibuffer-prompt))

(fset #'yes-or-no-p #'y-or-n-p)

(show-paren-mode t)
(window-divider-mode t)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(minibuffer-depth-indicate-mode t)

(my:with-package vi-tilde-fringe
  :ensure t
  :init (add-hook 'my:first-frame-hook
                  #'(lambda ()
                      (when (fboundp 'define-fringe-bitmap)
                        (global-vi-tilde-fringe-mode)))))

(my:with-package minions
  :ensure t
  :init (progn
          (setq-default minions-direct '(flycheck-mode projectile-mode))
          (minions-mode 1)))


(provide 'init-ui)

;;; init-ui.el ends here
