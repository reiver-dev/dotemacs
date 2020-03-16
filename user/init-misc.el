;;; init-misc.el --- Uncategorized configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-defs)
(require 'init-keybind)
(require 'init-package)
(require 'init-process)


(defconst -my:gc-threshold 800000
  "Default value for `gc-cons-threshold'.")

;; enable all commands
(setq-default disabled-command-function nil)


(my:after warning
  (add-to-list 'warning-suppress-types '(undo discard-info)))


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


(defun my:bounds ()
  "Return region start and end of region.
If regions is not active, then return buffer bounds."
  (interactive)
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (if (<= start end)
            (list start end)
          (list end start)))
    (list (point-min) (point-max))))


(defun my:kill-current-position (&optional arg)
  "Add current buffer:line possition to kill ring.
With optional ARG use buffer base name."
  (interactive "P")
  (kill-new
   (message "%s:%d"
            (if arg (buffer-file-name)
              (file-name-base (buffer-file-name)))
            (point))))


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


(defun my:func-log-args (proc &rest args)
  "Call PROC with ARGS and log it's arguments and result."
  (let ((procname (when proc (function-get proc 'name))))
    (unwind-protect
        (progn
          (message "TRACE %s %s begin" procname args)
          (let ((result (apply proc args)))
            (message "TRACE %s %s -> %s" procname args result)
            result))
      (message "TRACE %s %s finish" procname args))))


;; Sync unchanged buffers with filesystem
(my:with-package autorevert
  :defer 0.5
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


;; Help buffer

(my:with-package helpful
  :ensure t
  :init (my:kmap
         ([remap describe-function] #'helpful-callable)
         ([remap describe-variable] #'helpful-variable)
         ([remap describe-symbol] #'helpful-symbol)
         ([remap describe-key] #'helpful-key)))


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


(defun my:reindent-json-2 (start end)
  "Use python to format json.
Honors initial offset. START and END are text bounds in current
buffer."
  (interactive (my:bounds))
  (let ((code
         (mapconcat
          #'identity
          '("from json import loads, dumps"
            "from sys import stdin, stdout, stderr"
            "from textwrap import indent"
            "data = stdin.read()"
            "prefix = len(data) - len(data.lstrip())"
            "suffix = len(data) - len(data.rstrip())"
            "offset = ' ' * prefix"
            "result = indent(dumps(loads(data), indent=2), offset)"
            "stdout.write(result)"
            "stdout.write(data[len(data) - suffix:])")
          "\n")))
    (my:process-region start end "python" "-c" code)))


(provide 'init-misc)

;;; init-misc.el ends here
