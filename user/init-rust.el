;;; init-rust.el --- Rust language configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-completion)


(defconst -my:rust-has-racer (executable-find "racer"))


(my:with-package racer
  :if -my:rust-has-racer
  :ensure t)

(my:with-package company-racer
  :if -my:rust-has-racer
  :ensure t
  :init (progn
          (add-to-list 'company-backends #'company-racer)))

(my:with-package flycheck-rust
  :if -my:rust-has-racer
  :ensure t)



(provide 'init-rust)

;;; init-rust.el ends here
