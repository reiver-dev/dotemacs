;;; init-rust.el --- Rust language configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-completion)


(defconst -my:rust-has-racer (executable-find "racer"))
(defconst -my:rust-has-cargo (executable-find "cargo"))
(defconst -my:rust-has-rls (executable-find "rls"))


(my:with-package rust-mode
  :ensure t
  :init (setq-default rust-indent-where-clause t))

(my:with-package lsp-rust
  :ensure t
  :init (my:after rust-mode (require 'lsp-rust))
  :config (add-hook 'rust-mode-hook #'lsp-rust-enable))


(my:with-package cargo
  :if -my:rust-has-cargo
  :ensure t)


(provide 'init-rust)

;;; init-rust.el ends here
