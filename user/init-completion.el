;;; init-completion.el --- Completion framewords  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'init-package)
(require 'init-keybind)


(defconst my:user-dir (file-name-directory
                       (or load-file-name buffer-file-name)))


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


(defun my:hippie-expand-no-case-fold ()
  "Call `hippie-expand' but without `case-fold-search'."
  (interactive)
  (let ((case-fold-search nil))
    (call-interactively #'hippie-expand)))


(defun my:hippie-expand-files ()
  "Call `hippie-expand' for filename completion."
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-complete-file-name-partially
           try-complete-file-name)))
    (call-interactively #'hippie-expand)))


(my:kmap
 ([remap dabbrev-expand] #'hippie-expand) ; "M-/"
 ([remap dabbrev-completion] #'my:hippie-expand-files)) ; "C-M-/"


(my:with-package ivy
  :ensure t
  :defer 1
  :init (ivy-mode t))


(my:with-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


(my:with-package counsel
  :ensure t
  :init (my:after ivy (counsel-mode t))
  :config (progn
            (require 'init-ivy)
            (my:kmap* counsel-mode-map
                      ([remap yank-pop] nil)
                      ("C-M-y" #'counsel-yank-pop))))


;; Completion
(my:with-package company
  :ensure t
  :defer 1
  :init (global-company-mode t)
  :config (progn
            (require 'init-company)
            (my:kmap* company-mode-map
                      ("C-<tab>" #'company-complete))))


(my:with-package yasnippet
  :ensure t
  :defer t
  :init (yas-global-mode t)
  :config (progn
            ;; No more toolkit popups
            (setq-default
             yas-prompt-functions
             '(yas-ido-prompt yas-completing-prompt yas-no-prompt))
            ;; Just custom snippet dir
            (add-hook 'term-mode-hook
                      (lambda () (yas-minor-mode -1)))))


(my:with-package yasnippet-snippets
  :ensure t)


(provide 'init-completion)

;;; init-completion.el ends here
