;;; init-export.el --- Hooks at buffer export -*- lexical-binding: t -*-

;;; Commentary:
;; This module adds helpers to change buffer state at
;; buffer export like org-mode html export, htmlize.

;;; Code:


(defvar -my:exporting-in-progress nil
  "Bound when code exporting is performed.")


(defvar my:exporting-hook nil
  "Called as mode hook when buffer is exported.")


(defun my:exporting-p ()
  "Check if buffer export is currently performed."
  -my:exporting-in-progress)


(defun my:exporting-wrap (proc &rest args)
  "Bind that export is in progress and call PROC.
Argument list ARGS is applied to the PROC.
This function is meant to be used as an advice
around exporting functions."
  (let ((-my:exporting-in-progress t))
    (apply proc args)))


(defun my:exporting-buffer-setup nil
  "Run exporting hooks when needed."
  (when (my:exporting-p)
    (run-hooks 'my:exporting-hook)))


(add-hook 'prog-mode-hook #'my:exporting-buffer-setup)
(add-hook 'text-mode-hook #'my:exporting-buffer-setup)


(provide 'init-export)

;;; init-export.el ends here
