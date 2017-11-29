;;; init-org.el --- Org-Mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This files provides customizations and
;; function used in org-mode

;;; Code:

(eval-when-compile
  (require 'init-package)
  (require 'init-list))


(setq-default org-src-fontify-natively t
              org-src-tab-acts-natively t
              org-src-window-setup 'current-window
              org-startup-folded nil
              org-use-sub-superscripts (quote {})
              ;; set maximum indentation for description lists
              org-list-description-max-indent 5
              ;; prevent demoting heading also shifting text inside sections
              org-adapt-indentation nil)


(defun -my:org-add-language (&rest langs)
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append (my:remove-if (lambda (l) (memq (car l) langs))
                           org-babel-load-languages)
             (mapcar (lambda (l) (cons l t)) langs))))


(my:after 'org
  (-my:org-add-language 'python 'plantuml))


(my:after 'plantuml-mode
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))


(my:with-package ob-ipython
  :if (executable-find "jupyter")
  :ensure t
  :init (my:after 'org
          (-my:org-add-language 'ipython)))


(provide 'init-org)

;;; init-org.el ends here
