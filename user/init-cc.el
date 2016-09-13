;;; init-cc.el --- C/C++ configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'init-package)
(require 'init-completion)


(defconst my:c-style
  '("linux"
    (c-basic-offset . 4)
    (c-offsets-alist
     (innamespace . 0)
     (inline-open . 0)
     (inextern-lang . 0))))


(with-eval-after-load 'cc-mode
  (c-add-style "reiver" my:c-style)
  (setq-default c-default-style
                '((c-mode . "reiver")
                  (c++-mode . "reiver")
                  (java-mode . "java")
                  (awk-mode . "awk")
                  (other . "gnu"))))


(when (executable-find "clang")
  (my:with-package irony
    :ensure t
    :init (progn
            (add-hook 'c++-mode-hook 'irony-mode)
            (add-hook 'c-mode-hook 'irony-mode)))

  (my:with-package company-irony
    :ensure t
    :init (add-to-list 'company-backends 'company-irony))

  (my:with-package flycheck-irony
    :ensure t
    :init (with-eval-after-load 'flycheck
            (flycheck-irony-setup)))

  (my:with-package irony-eldoc
    :ensure t
    :init (add-hook 'irony-mode-hook 'irony-eldoc)))


(my:with-package company-c-headers
  :ensure t
  :config (add-to-list 'company-backends #'company-c-headers))


(my:with-package ggtags
  :if (executable-find "gtags")
  :ensure t
  :init (progn
          (add-hook 'c-mode-hook 'ggtags-mode)
          (add-hook 'c++-mode-hook 'ggtags-mode))
  :config (my:kmap* ggtags-mode-map
                    ("M-." "C-M-." "M-*" "M-," nil)
                    ("M-." #'ggtags-find-tag-dwim)
                    ("C-M-." #'ggtags-find-tag-regexp)))


(provide 'init-cc)

;;; init-cc.el ends here
