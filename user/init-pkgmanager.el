;;; init-pkgmanager.el --- Bootstraping package managers  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'init-package)


(my:with-package quelpa
  :ensure t
  :init (progn
          (setq-default quelpa-update-melpa-p nil)))


(provide 'init-pkgmanager)


;;; init-pkgmanager.el ends here
