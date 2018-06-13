;;; init-dired.el --- Main config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(eval-when-compile
  (require 'init-package))


(setq-default dired-listing-switches "-lhvA"
              dired-clean-up-buffers-too t
              dired-recursive-copies 'always
              dired-recursive-deletes 'top
              dired-hide-details-hide-symlink-targets nil)


(defun -my:dired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(advice-add 'dired-readin :after #'-my:dired-sort)


(my:with-package dired+
  :ensure t
  :init (progn
          (setq-default
           diredp-hide-details-initially-flag t
           diredp-hide-details-propagate-flag t)
          (my:after dired
            (let ((inhibit-message t))
              (load "dired+.el"))))
  :config (let ((inhibit-message t))
            (diredp-toggle-find-file-reuse-dir t)))


(provide 'init-dired)

;;; init-dired.el ends here


