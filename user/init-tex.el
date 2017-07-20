;;; init-tex.el --- TeX configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-completion)

(defconst -my:tex-present (executable-find "tex"))


(my:with-package auctex
  :if -my:tex-present
  :ensire t
  :init (progn
          (setq TeX-auto-parse t
                TeX-parse-self t)
          (setq-default TeX-master nil)))

(my:with-package company-auctex
  :if -my:tex-present
  :ensure t
  :init (my:after 'company-mode
          (add-to-list 'company-backends #'company-auctex)))


(provide 'init-tex)

;;; init-clojure.el ends here

