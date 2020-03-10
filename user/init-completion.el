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
  :disabled t
  :ensure t
  :init (setq completing-read-function 'ivy-completing-read
              completion-in-region-function 'ivy-completion-in-region))


(my:with-package ivy-xref
  :disabled t
  :ensure t
  :init (setq-default xref-show-xrefs-function 'ivy-xref-show-xrefs))


(my:with-package counsel
  :disabled t
  :ensure t
  :init (my:after ivy (counsel-mode t))
  :config (progn
            (require 'init-ivy)
            (my:kmap* counsel-mode-map
                      ([remap yank-pop] nil)
                      ("C-M-y" #'counsel-yank-pop))))


(my:with-package selectrum
  :init (progn
          (quelpa '(selectrum :repo "raxod502/selectrum" :fetcher github))
          (selectrum-mode +1)))


(my:with-package selectrum-prescient
  :init (progn (quelpa '(selectrum-prescient
                         :repo "raxod502/prescient.el"
                         :fetcher github))
               (selectrum-prescient-mode +1)))


;; Completion
(my:with-package company
  :ensure t
  :defer 0.5
  :init (require 'company)
  :config (progn
            (setq-default
             company-backends '(company-capf
                                company-files
                                (company-dabbrev-code company-keywords)
                                company-dabbrev)
             company-frontends '(company-pseudo-tooltip-frontend)
             company-idle-delay 0.15
             company-minimum-prefix-length 1
             company-tooltip-minimum company-tooltip-limit
             company-require-match #'company-explicit-action-p
             company-tooltip-align-annotations t
             company-dabbrev-code-other-buffers nil
             company-dabbrev-downcase nil
             company-dabbrev-ignore-case nil)
            (my:kmap* company-active-map
                      ("TAB" "<tab>" #'company-complete-selection)
                      ("C-p" #'company-select-previous)
                      ("C-n" #'company-select-next))
            (my:kmap ("C-<tab>" #'company-complete))
            (global-company-mode +1)))


(my:with-package company-prescient
  :ensure t
  :init (progn (company-prescient-mode +1)))


(my:with-package yasnippet
  :ensure t
  :defer 10
  :init (yas-global-mode t)
  :config (progn
            ;; No more toolkit popups
            (defun my:yas-ivy-prompt (prompt choices &optional display-fn)
              (yas-completing-prompt prompt choices
                                     display-fn #'ivy-completing-read))
            (setq-default
             yas-prompt-functions
             '(my:yas-ivy-prompt yas-completing-prompt yas-no-prompt))
            ;; Just custom snippet dir
            (add-hook 'term-mode-hook
                      (lambda () (yas-minor-mode -1)))))


(my:with-package yasnippet-snippets
  :ensure t)


(provide 'init-completion)

;;; init-completion.el ends here
