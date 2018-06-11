;;; init-lsp.el --- LSP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module configures Language server protocol

;;; Code:


(my:with-package lsp-mode
  :ensure t
  :config (progn
            (require 'lsp-imenu)
            (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)))


(my:with-package company-lsp
  :ensure t
  :init (my:after (lsp-mode company)
          (add-to-list 'company-backends 'company-lsp)))


(provide 'init-lsp)

;;; init-lsp.el ends here

