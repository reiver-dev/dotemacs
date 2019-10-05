;;; init-process.el --- Running subprocesses -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setq shell-command-default-error-buffer "*Shell Command STDERR*")


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


(provide 'init-process)

;;; init-process.el ends here
