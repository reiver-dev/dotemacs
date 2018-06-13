;;; init-misc.el --- Uncategorized configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-defs)
(require 'init-package)


(defconst -my:gc-threshold 800000
  "Default value for `gc-cons-threshold'.")

;; enable all commands
(setq-default disabled-command-function nil)

(my:when-windows
  (setq inhibit-compacting-font-caches t
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)))

(defun -my:gc-disable ()
  "Set `gc-cons-threshold' to `most-positive-fixnum'."
  (setq gc-cons-threshold most-positive-fixnum))

(defun -my:gc-enable ()
  "Setq `gc-cons-threshold' to default value."
  (setq gc-cons-threshold -my:gc-threshold))

(add-hook 'minibuffer-setup-hook #'-my:gc-disable)
(add-hook 'minibuffer-exit-hook #'-my:gc-enable)

;; Sync unchanged buffers with filesystem
(my:with-package autorevert
  :init (progn
          ;; Disable `Reverting buffer' messages
          (setq-default auto-revert-verbose nil)
          (global-auto-revert-mode t))
  :config
  (progn
    (defun my:log-tail-handler ()
      "Setup current buffer to view logs efficiently."
      (setq-local auto-revert-interval 1)
      (auto-revert-set-timer)
      (read-only-mode t)
      (font-lock-mode 0)
      (goto-char (point-max)))
    (add-hook 'auto-revert-tail-mode-hook #'my:log-tail-handler)))


;; Show recent files
(my:with-package recentf
  :defer t
  :init (recentf-mode t))


(setq shell-command-default-error-buffer "*Shell Command STDERR*")

;; External tools
(defun my:process-region-with-command (command)
  "Execute COMMAND string over active region or entire buffer."
  (let (begin end noncont)
    (if (region-active-p)
        (setq begin (region-beginning)
              end (region-end)
              noncont (region-noncontiguous-p))
      (setq begin (point-min)
            end (point-max)
            noncont nil))
    (when (< begin end)
      (shell-command-on-region
       begin end command
       ;; no buffer set, replace in current
       nil t
       ;; display error-bufffer
       shell-command-default-error-buffer t
       noncont))))


(defun my:reindent-xml ()
  "Use xmllint to format XML."
  (interactive)
  (my:process-region-with-command "xmllint --format -"))


(defun my:reindent-html ()
  "Use xmllint to format HTML."
  (interactive)
  (my:process-region-with-command "xmllint --format --html -"))


(defun my:reindent-json ()
  "Use python to format json."
  (interactive)
  (my:process-region-with-command "python -m json.tool"))


(provide 'init-misc)

;;; init-misc.el ends here
