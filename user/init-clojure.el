;;; init-clojure.el --- Clojure configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-parens)


(my:with-package clojure-mode
  :ensure t
  :config (add-hook 'clojure-mode-hook #'my:paredit-extended-mode))


(my:with-package cider
  :if (executable-find "lein")
  :ensure t
  :config (progn
            (add-hook 'cider-repl-mode-hook
                      #'my:paredit-mode)
            (add-hook 'cider-repl-mode-hook
                      #'my:paredit-extended-mode)))


(provide 'init-clojure)

;;; init-clojure.el ends here
