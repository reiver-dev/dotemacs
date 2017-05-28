;;; init-org.el --- Org-Mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)


(setq-default org-ellipsis "  "
              org-src-fontify-natively t
              org-src-tab-acts-natively t
              org-src-window-setup 'current-window
              ;; set maximum indentation for description lists
              org-list-description-max-indent 5
              ;; prevent demoting heading also shifting text inside sections
              org-adapt-indentation nil)


(my:after 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))


(my:with-package ob-ipython
  :ensure t
  :init (my:after 'org
          (org-babel-do-load-languages
           'org-babel-load-languages
           '((ipython . t)))))


(provide 'init-org)

;;; init-org.el ends here
