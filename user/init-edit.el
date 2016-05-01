;;; init-edit.el --- Editing routines -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my:push-mark-no-activate ()
  "Calls `push-mark' like `push-mark-command' but withoug activation"
  (interactive)
  (push-mark))

;; Do not activate mark during jump
(defun my:exchange-point-and-mark (&optional ARG)
  "Inverse `exchange-point-and-mark' prefix argument
 when mark is not active (`mark-active')"
  (interactive "P")
  (exchange-point-and-mark
   (if mark-active ARG (not ARG))))

(defun my:kill-line-to-indent ()
  "Kills line backward (opposite to `kill-line')
and indents after that"
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

;; Region dependent choices
(defun my:kill-region-or-word (arg)
  "Call `kill-region' or `backward-kill-word'
depending on whether or not a region is selected."
  (interactive "*p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun my:delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my:backward-delete-word (arg)
  (interactive "p")
  (my:delete-word (- arg)))

(unless (fboundp 'upcase-dwim)
  (defun upcase-dwim (arg)
    "Call `upcase-region' or `upcase-word'
depending on whether or not a region is selected."
    (interactive "*p")
    (if (use-region-p)
        (upcase-region (region-beginning) (region-end))
      (upcase-word arg))))

(unless (fboundp 'downcase-dwim)
  (defun downcase-dwim (arg)
    "Call `downcase-region' or `downcase-word'
depending on whether or not a region is selected."
    (interactive "*p")
    (if (use-region-p)
        (downcase-region (region-beginning) (region-end))
      (downcase-word arg))))

(unless (fboundp 'capitalize-dwim)
  (defun capitalize-dwim (arg)
    "Call `capitalize-region' or `capitalize-word'
depending on whether or not a region is selected."
    (interactive "*p")
    (if (use-region-p)
        (capitalize-region (region-beginning) (region-end))
      (capitalize-word arg))))

(defun my:join-line (&optional ARG)
  "Backward from `delete-indentation'.
Joins this line to following line.
With argument, join this line to previous line"
  (interactive "P")
  (delete-indentation (unless ARG t)))

(defun my:move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun my:move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun my:minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode,
 if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (deactivate-mark)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my:kill-buffer ()
  "Kills current active buffer without prompt"
  (interactive)
  (kill-buffer (current-buffer)))

(defun my:kill-buffer-and-window ()
  "Kills current active buffer without prompt, closes window too"
  (interactive)
  (let ((buf (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (kill-buffer buf)))

(defun my:hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (call-interactively #'hippie-expand)))

(defun my:hippie-expand-files ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-complete-file-name-partially
           try-complete-file-name)))
    (call-interactively #'hippie-expand)))


(provide 'init-edit)

;;; init-edit.el ends here
