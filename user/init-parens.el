;;; init-parens.el --- Parentheses and wrapping  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-keybind)
(require 'init-package)



(defvar my:paredit-extended-mode-map (make-sparse-keymap)
  "Keymap for `my:paredit-exteded-mode'.")

(define-minor-mode my:paredit-extended-mode
  "Sets from `smartparens-mode': \\{my:paredit-extended-mode-map}"
  :keymap my:paredit-extended-mode-map)

(add-hook 'lisp-mode-hook #'my:paredit-extended-mode)
(add-hook 'emacs-lisp-mode-hook #'my:paredit-extended-mode)


(my:with-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


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
            (add-hook 'my:paredit-extended-mode-hook #'smartparens-strict-mode)))


(provide 'init-parens)

;;; init-parens.el ends here
