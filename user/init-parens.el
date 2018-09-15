;;; init-parens.el --- Parentheses and wrapping  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-keybind)
(require 'init-package)


(defvar my:paredit-mode-map (make-sparse-keymap)
  "Keymap for `my:paredit-mode'.")

(defvar my:paredit-extended-mode-map (make-sparse-keymap)
  "Keymap for `my:paredit-exteded-mode'.")

(define-minor-mode my:paredit-mode
  "Mode to store paren editing bindings: \\{my:paredit-mode-map}"
  :keymap my:paredit-mode-map)

(define-minor-mode my:paredit-global-mode
  "Global mode to store paren editing bindings: \\{my:paredit-mode-map}"
  :global t
  :keymap my:paredit-mode-map)

(define-minor-mode my:paredit-extended-mode
  "Mode to store lisp paren editing bindings: \\{my:paredit-extended-mode-map}"
  :keymap my:paredit-extended-mode-map)


(defun my:-paredit-bind-simple (map)
  "Set simple `paredit-mode-mode' functions into MAP keymap."
  (my:kmap*
   map
   ("C-x p (" "C-x p 9" 'paredit-wrap-round)
   ("C-x p [" 'paredit-wrap-square)
   ("C-x p {" 'paredit-wrap-curly)
   ("C-x p ," "C-x p <" 'paredit-wrap-angled)

   ("C-x p a" 'paredit-raise-sexp)
   ("C-x p b" 'paredit-splice-sexp-killing-backward)
   ("C-x p f" 'paredit-splice-sexp-killing-forward)

   ("C-x p r" 'paredit-splice-sexp) ;; raise/unwrap
   ("C-x p s" 'paredit-split-sexp)
   ("C-x p j" 'paredit-join-sexps)
   ("C-x p c" 'paredit-convolute-sexp)))


(defun my:-paredit-bind-extended (map)
  "Set lisp-only `paredit-mode-mode' functions into MAP keymap."
  (my:kmap*
   map
   ;; Inserting
   ("(" 'paredit-open-round)
   (")" 'paredit-close-round)
   ("[" 'paredit-open-square)
   ("]" 'paredit-close-square)
   ("{" 'paredit-open-curly)
   ("}" 'paredit-close-curly)

   ("\"" 'paredit-doublequote)
   ("M-\"" 'paredit-meta-doublequote)
   ("\\" 'paredit-backslash)
   (";" 'paredit-semicolon)
   ("C-j" 'paredit-newline)
   ([remap comment-dwim] 'paredit-comment-dwim)

   ;; Safe killing
   ([remap delete-char]
    [remap delete-forward-char]
    'paredit-forward-delete)

   ([remap delete-backward-char]
    [remap backward-delete-char-untabify]
    'paredit-backward-delete)

   ([remap kill-line] 'paredit-kill)

   ([remap my:delete-word]
    [remap kill-word]
    'paredit-forward-kill-word)

   ([remap my:backward-delete-word]
    [remap backward-kill-word]
    'paredit-backward-kill-word)

   ;; Moving around
   ("C-M-f" 'paredit-forward)
   ("C-M-b" 'paredit-backward)
   ("C-M-u" 'paredit-backward-up)
   ("C-M-d" 'paredit-forward-down)
   ("C-M-p" 'paredit-backward-down)
   ("C-M-n" 'paredit-forward-up)

   ;; Slurping and barfing
   ("C-<right>" 'paredit-forward-slurp-sexp)
   ("C-<left>" 'paredit-forward-barf-sexp)
   ("C-M-<right>" 'paredit-backward-barf-sexp)
   ("C-M-<left>" 'paredit-backward-slurp-sexp)))

(add-hook 'prog-mode-hook #'my:paredit-mode)
(add-hook 'lisp-mode-hook #'my:paredit-extended-mode)
(add-hook 'emacs-lisp-mode-hook #'my:paredit-extended-mode)

(my:with-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(my:with-package paredit
  :ensure t
  :init (require 'paredit)
  :config (progn
            (my:-paredit-bind-simple my:paredit-mode-map)
            (my:-paredit-bind-extended my:paredit-extended-mode-map)))

(provide 'init-parens)

;;; init-parens.el ends here
