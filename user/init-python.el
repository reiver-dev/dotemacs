;;; init-python.el --- Python configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-completion)


;; Replace python.el version to support venv on windows

(defconst -my:python-venv-bin (if (eq system-type 'windows-nt)
                                  "Scripts" "bin"))

(with-eval-after-load 'python
  (defun -my:python-shell-calculate-exec-path ()
    "Calculate `exec-path'.
Prepends `python-shell-exec-path' and adds the binary directory
for virtualenv if `python-shell-virtualenv-root' is set.  If
`default-directory' points to a remote host, the returned value
appends `python-shell-remote-exec-path' instead of `exec-path'."
    (let ((new-path (copy-sequence
                     (if (file-remote-p default-directory)
                         python-shell-remote-exec-path
                       exec-path))))
      (python-shell--add-to-path-with-priority
       new-path python-shell-exec-path)
      (if (not python-shell-virtualenv-root)
          new-path
        (python-shell--add-to-path-with-priority
         new-path
         (list (expand-file-name -my:python-venv-bin
                                 python-shell-virtualenv-root)))
        new-path)))
  (fset 'python-shell-calculate-exec-path
        '-my:python-shell-calculate-exec-path))


(my:with-package anaconda-mode
  :disabled t
  :ensure t
  :init (add-hook 'python-mode-hook #'anaconda-mode)
  :config (progn
            (my:kmap* anaconda-mode-map
                      ("M-*" "M-," "M-." "C-M-i" nil)
                      ("M-." #'anaconda-mode-find-definitions)
                      ("M-," #'anaconda-mode-go-back)
                      ("C-M-." #'anaconda-mode-find-assignments)
                      ("M-]" #'anaconda-mode-find-references)
                      ([remap completion-at-point] #'anaconda-mode-complete))))


(my:with-package company-anaconda
  :disabeled t
  :ensure t
  :init (with-eval-after-load 'anaconda-mode
          (add-to-list 'company-backends #'company-anaconda)))

(my:with-package python-environment
  :ensure t
  :config (progn
            (setq python-environment-virtualenv
                  (append (list "python" "-m") python-environment-virtualenv))))

(my:with-package jedi-core
  :ensure t
  :init (add-hook 'python-mode-hook 'jedi:setup)
  :config (my:kmap* jedi-mode-map
                    ("M-." #'jedi:goto-definition)
                    ("M-," #'jedi:goto-definition-pop-marker)))

(my:with-package company-jedi
  :ensure t
  :init (with-eval-after-load 'jedi-core
          (add-to-list 'company-backends #'company-jedi)))


(provide 'init-python)

;;; init-python.el ends here
