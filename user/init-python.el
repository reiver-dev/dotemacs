;;; init-python.el --- Python configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-defs)
(require 'init-package)
(require 'init-completion)

;; Replace python.el version to support venv on windows

(setq-default python-shell-completion-native-enable
              (not (eq system-type 'windows-nt)))


(my:after 'python
  (require 'init-python-internal))


(my:with-package anaconda-mode
  :ensure t
  :init (add-hook 'python-mode-hook 'anaconda-mode)
  :config (progn
            (my:kmap* anaconda-mode-map
                      ("M-r" "M-*" "M-," "M-." nil)
                      ("M-." #'anaconda-mode-find-definitions)
                      ("C-M-." #'anaconda-mode-find-assignments)
                      ("M-?" #'anaconda-mode-find-references)
                      ("C-c C-d" #'anaconda-mode-show-doc)
                      ("M-," #'xref-pop-marker-stack))))


(my:with-package company-anaconda
  :ensure t
  :init (my:after '(anaconda-mode company)
          (add-to-list 'company-backends #'company-anaconda)))


(my:with-package jedi-core
  :disabled t
  :ensure t
  :init (add-hook 'python-mode-hook 'jedi:setup)
  :config (my:kmap* jedi-mode-map
                    ("M-." #'jedi:goto-definition)
                    ("M-," #'jedi:goto-definition-pop-marker)))

(my:with-package company-jedi
  :disabled t
  :ensure t
  :init (my:after jedi-core
          (add-to-list 'company-backends #'company-jedi)))


(provide 'init-python)

;;; init-python.el ends here
