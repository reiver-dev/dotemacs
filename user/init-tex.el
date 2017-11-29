;;; init-tex.el --- TeX configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-package)
  (require 'init-completion))

(defconst -my:tex-present (executable-find "tex"))


(my:with-package auctex
  :if -my:tex-present
  :ensire t
  :init (progn
          (setq-default TeX-auto-parse t
                        TeX-parse-self t
                        TeX-master nil)))

(my:with-package company-auctex
  :if -my:tex-present
  :ensure t
  :init (my:after 'company
          (my:after 'tex
            (company-auctex-init))))


(provide 'init-tex)

;;; init-clojure.el ends here

