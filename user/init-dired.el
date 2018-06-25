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

(my:after dired (require 'dired-x))


(my:with-package dired-k
  :ensure t
  :init (progn
          (setq-default dired-k-human-readable t)
          (my:after dired (require 'dired-k)))
  :config (progn
            (advice-add #'dired-k--highlight :around
                        #'-my:dired-k--highlight-if-local)
            (add-hook 'dired-initial-position-hook #'dired-k)
            (add-hook 'dired-after-readin-hook #'dired-k-no-revert)))


(defun -my:dired-k--highlight-if-local (orig-fn &rest args)
              "Butt out if the requested directory is remote."
              (unless (file-remote-p default-directory)
                (apply orig-fn args)))


(provide 'init-dired)

;;; init-dired.el ends here


