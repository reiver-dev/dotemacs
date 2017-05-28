;;; init-haskell.el --- C/C++ configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'init-package)
(require 'init-completion)


(my:with-package haskell-mode
  :ensure t
  :config (progn
            (add-hook 'haskell-mode-hook #'haskell-indentation-mode)))


(my:with-package company-ghc
  :if (executable-find "ghc-mod")
  :ensure t
  :config (my:after 'company-mode
            (add-to-list 'company-backends #'company-ghc)
            (add-hook 'haskell-mode-hook #'ghc-init)))



(provide 'init-haskell)

;;; init-haskell.el ends here
