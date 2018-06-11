;;; init-cedet.el --- CEDET configuration  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)


(my:after semantic

  (defun my:cedet-setup ()
    "Local settings for function `semantic-mode'."
    (local-set-key (kbd "C-c i") #'semantic-decoration-include-visit)
    (local-set-key (kbd "C-c j") #'semantic-ia-fast-jump)
    (local-set-key (kbd "C-c q") #'semantic-ia-show-doc)
    (local-set-key (kbd "C-c s") #'semantic-ia-show-summary)
    (local-set-key (kbd "C-c t") #'semantic-analyze-proto-impl-toggle))

  (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes
               'global-semantic-show-parser-state-mode)
  (add-to-list 'semantic-default-submodes
               'global-semantic-show-unmatched-syntax-mode)

  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)

  (add-hook 'c-mode-hook 'my:cedet-setup)
  (add-hook 'c++-mode-hook 'my:cedet-setup))

(my:after semantic/dep
  (defun my:system-include-path ()
    "Just returns `semantic-dependency-system-include-path'
to feed to other packages"
    semantic-dependency-system-include-path)
  (my:after company-c-headers
    (setq company-c-headers-path-system 'my:system-include-path)))


(provide 'init-cedet)

;;; init-cedet.el ends here
