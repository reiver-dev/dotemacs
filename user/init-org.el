;;; init-org.el --- Org-Mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-macro)


(setq-default org-ellipsis "⤵"
              org-hide-leading-stars t
              org-src-fontify-natively t
              org-src-tab-acts-natively t
              org-src-window-setup 'current-window
              ;; set maximum indentation for description lists
              org-list-description-max-indent 5
              ;; prevent demoting heading also shifting text inside sections
              org-adapt-indentation nil)


(my:with-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))


(provide 'init-org)

;;; init-org.el ends here
