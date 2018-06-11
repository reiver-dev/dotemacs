;;; init-eshell.el --- Eshell config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-package)
  (require 'init-environ))


(my:with-package eshell
  :init (setq-default eshell-scroll-to-bottom-on-input t)
  :config
  (progn
    (defun eshell/ff (&rest files)
      "Alias for `eshell' to `find-file' FILES."
      (dolist (f files)
        (find-file f t)))

    (defun eshell/fo (&rest files)
      "Alias for `eshell' to `find-file-other-window' for FILES."
      (dolist (f files)
        (find-file-other-window f t)))

    (defun eshell/compile (&rest args)
      (compile (combine-and-quote-strings args)))

    (defun eshell/vs-locate (version)
      (my:env-w32-vcvars-location version))

    (defun eshell/vs-apply (version &rest args)
      (apply #'my:env-w32-vcvars-apply version args)
      (setq eshell-path-env (getenv "PATH")))))



(provide 'init-eshell)

;;; init-eshell.el ends here
