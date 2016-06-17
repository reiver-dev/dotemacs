;;; init-org.el --- Org-Mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)


(setq-default org-ellipsis "⤵"
              org-src-fontify-natively t
              org-src-tab-acts-natively t
              org-src-window-setup 'current-window
              ;; set maximum indentation for description lists
              org-list-description-max-indent 5
              ;; prevent demoting heading also shifting text inside sections
              org-adapt-indentation nil)


(provide 'init-org)

;;; init-org.el ends here
