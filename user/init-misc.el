;;; init-misc.el --- Uncategorized configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-defs)
(require 'init-package)
(require 'init-process)


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


(defun my:set-vars (variables)
  "Apply associative list of VARIABLES.
Return old values as kv cons pairs if values changed. Coparison is
performed with `eq'."
  (let (result)
    (dolist (kv variables)
      (let ((symbol (car kv))
            (newval (cdr kv)))
        (when (boundp symbol)
          (let ((curval (symbol-value symbol)))
            (unless (eq curval newval)
              (setq result
                    (cons (cons symbol curval)
                          result))
              (set symbol newval))))))
    result))


(defun my:set-modes (modes)
  "Change state of MODES passed as an associative list.
Return old modes states if changed. Element is ignored if mode is not
loaded."
  (let (result)
    (dolist (kv modes)
      (let ((symbol (car kv))
            (newstate (cdr kv)))
        (when (and (boundp symbol) (fboundp symbol))
          (let ((func (symbol-function symbol))
                (curstate (symbol-value symbol)))
            (unless (eq newstate curstate)
              (setq result
                    (cons (cons symbol curstate)
                          result))
              (funcall func newstate))))))
    result))


;; Sync unchanged buffers with filesystem
(my:with-package autorevert
  :init (progn
          ;; Disable `Reverting buffer' messages
          (setq-default auto-revert-verbose nil)
          (global-auto-revert-mode t))
  :config (progn
            (defun -my:log-tail-handler ()
              "Setup current buffer to view logs efficiently."
              (setq-local auto-revert-interval 1)
              (auto-revert-set-timer)
              (read-only-mode t)
              (font-lock-mode -1)
              (goto-char (point-max)))
            (add-hook 'auto-revert-tail-mode-hook '-my:log-tail-handler)))


(defun my:describe-coding-vars ()
  "Display current encoding variables and their values."
  (interactive)
  (with-output-to-temp-buffer (help-buffer)
    (dolist (s
             '(current-language-environment
               buffer-file-coding-system
               default-file-name-coding-system
               default-keyboard-coding-system
               default-process-coding-system
               default-sendmail-coding-system
               default-terminal-coding-system
               epg-passphrase-coding-system
               file-name-coding-system
               inherit-process-coding-system
               keyboard-coding-system
               last-next-selection-coding-system
               locale-coding-system
               next-selection-coding-system
               recentf-save-file-coding-system
               rmail-file-coding-system
               save-buffer-coding-system
               selection-coding-system
               sendmail-coding-system
               vc-git-commits-coding-system
               vc-git-log-output-coding-system
               w32-system-coding-system))
      (princ
       (format "%s: %s\n"
               (symbol-name s)
               (when (boundp s) (symbol-value s)))))))


;; Show recent files
(my:with-package recentf
  :defer t
  :init (recentf-mode t))


;; External tools

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
