;;; init-ivy.el --- ivy-mode customization  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module is to be loaded after ivy-mode

;;; Code:

(eval-when-compile
  (require 'init-filesystem)
  (require 'ivy))


(defun my:counsel-complete-files (&optional initial-input)
  (interactive)
  (let ((existing (my:find-path-at-point)))
    ;; (when existing
    ;;   (setq ivy-completion-beg (+ 1 (match-beginning 0)))
    ;;   (setq ivy-completion-end (match-end 0)))
    (ivy-read "Complete file: " 'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action
              (lambda (x)
                (with-ivy-window
                  (ivy-completion-in-region-action
                   (file-relative-name x existing))))
              :preselect existing
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-find-file)))


(autoload 'counsel--find-file-matcher "counsel")


(setq-default ivy-display-functions-alist nil
              ivy-use-virtual-buffers t)


(provide 'init-ivy)

;;; init-ivy.el ends here
