;;; init-defs.el --- Main defs for user config  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defgroup my:init nil
  "Options for user init config."
  :group 'initialization)


(defgroup my:faces nil
  "Appearance-related user config"
  :group 'faces)



(defmacro my:when-windows (&rest body)
  "Evaluate BODY if current os is windows."
  (if (eq system-type 'windows-nt) (cons 'progn body)))


(defmacro my:when-posix (&rest body)
  "Evaluate BODY if current os is posix (not windows)."
  (if (eq system-type 'windows-nt) nil (cons 'progn body)))


(put 'my:when-windows 'lisp-indent-function 'defun)
(put 'my:when-posix 'lisp-indent-function 'defun)


(defvaralias 'my:first-frame-hook 'init:first-frame-hook)


(provide 'init-defs)

;;; init-defs.el ends here
