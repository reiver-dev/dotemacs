;;; init-comint.el --- Comint config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(eval-when-compile
  (require 'init-package)
  (require 'init-keybind))


(setq-default comint-prompt-read-only t
              comint-process-echoes t
              comint-input-ignoredups t
              comint-scroll-show-maximum-output t
              comint-scroll-to-bottom-on-input t
              comint-scroll-to-bottom-on-output nil
              comint-buffer-maximum-size 8196)


(my:after comint
  (defun -my:comint-text-readonly (_text)
    (let ((inhibit-read-only t)
          (output-end (process-mark (get-buffer-process (current-buffer)))))
      (put-text-property comint-last-output-start output-end 'read-only t)))
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (add-hook 'comint-output-filter-functions '-my:comint-text-readonly)
  ;; We have `my:kill-region-or-word' already
  (my:kmap* comint-mode-map ("C-c C-w" nil)))


(my:after shell
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))


(provide 'init-comint)

;;; init-comint.el ends here
