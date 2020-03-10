;;; init-pkgmanager.el --- Bootstraping package managers  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'init-package)


(my:with-package quelpa
  :ensure t)


(provide 'init-pkgmanager)


;;; init-pkgmanager.el ends here
