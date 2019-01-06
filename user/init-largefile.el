;;; init-largefile.el --- Treaks for big files -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun my:large-file-p ()
  "Check if current buffer is considered large.
See `large-file-warning-threshold'."
  (< large-file-warning-threshold (buffer-size)))

(define-derived-mode my:large-file-mode fundamental-mode "LargeFile"
  "Mode to minimize large file freezes"
  (setq-local bidi-display-reordering nil))


(add-to-list 'magic-mode-alist
             (cons #'my:large-file-p #'my:large-file-mode))


(provide 'init-largefile)

;;; init-largefile.el ends here
