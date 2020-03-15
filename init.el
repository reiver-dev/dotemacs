;;; init.el --- Startup file  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'init-base (expand-file-name "user/init-base.el"
                                      user-emacs-directory))


(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))
  (when (< emacs-major-version 27)
    (package-initialize))
  (load custom-file t)
  (require 'init-main)
  (load init:after-file t))


(if (daemonp)
    (add-hook 'after-make-frame-functions #'init:at-first-frame-function)
  (progn
    (run-hooks 'init:first-frame-hook)
    (init:at-frame-function (selected-frame))))


(add-hook 'after-make-frame-functions #'init:at-frame-function)


(provide 'init-el)

;;; init.el ends here
