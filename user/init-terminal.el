;;; init-terminal.el --- Embedded terminal buffers  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(eval-when-compile
  (require 'init-defs))

(require 'init-keybind)
(require 'init-package)


(my:when-posix
  (my:with-package vterm
    :ensure t
    :init (progn
            (setq-default vterm-keymap-exceptions
                          '("M-x" "C-x" "C-c" "C-u" "C-g"
                            "C-o" "C-v" "M-v" "C-y")))
    :config (progn
              (my:kmap* vterm-mode-map
                        ("C-o C-o" #'vterm-send-C-o)))))


(provide 'init-terminal)

;;; init-terminal.el ends here
