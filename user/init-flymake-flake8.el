;;; init-flymake-flake8.el --- Flymake flake8 checker  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'python)
(require 'flymake)


(defconst my:python-flake8-command
  (list "python" "-m" "flake8" "-"))


(defconst my:python-flake8-msg-alist
  '(("\\(^redefinition\\|.*unused.*\\|used$\\)" . :warning)
    ("^E9.*" . :error)
    ("^F8[23].*" . :error)
    ("^D.*" . :note)
    ("^N.*" . :note)
    ("^[EW][0-9]+" . :note)))


(declare-function -my:python-flymake "init-flymake-flake8.el")

(unless (fboundp '-my:python-flymake)
  (fset '-my:python-flymake (symbol-function 'python-flymake)))


(defun my:flymake-python-flake8 (report-fn &rest args)
  "Execute flake8 contining with REPORT-FN and ARGS."
  (let ((python-flymake-command my:python-flake8-command)
        (python-flymake-msg-alist my:python-flake8-msg-alist))
    (python-shell-with-environment
      (apply #'-my:python-flymake report-fn args))))


(fset 'python-flymake #'my:flymake-python-flake8)


(provide 'init-flymake-flake8)

;;; init-flymake-flake8.el ends here
