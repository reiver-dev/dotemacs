;;; init-edit.el --- Editing routines -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my:region-active ()
  "Checks whether there is valid active region selection"
  (and transient-mark-mode mark-active
       (not (eq (region-beginning) (region-end)))))

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
(defun my:kill-region-or-word ()
  "Call `kill-region' or `backward-kill-word'
depending on whether or not a region is selected."
  (interactive)
  (if (my:region-active)
      (call-interactively #'kill-region)
    (call-interactively #'backward-kill-word)))

(defun my:upcase-region-or-word ()
  "Call `upcase-region' or `upcase-word'
depending on whether or not a region is selected."
  (interactive)
  (if (my:region-active)
      (call-interactively #'upcase-region)
    (call-interactively #'upcase-word)))

(defun my:downcase-region-or-word ()
  "Call `downcase-region' or `downcase-word'
depending on whether or not a region is selected."
  (interactive)
  (if (my:region-active)
      (call-interactively #'downcase-region)
    (call-interactively #'downcase-word)))

(defun my:capitalize-region-or-word ()
  "Call `capitalize-region' or `capitalize-word'
depending on whether or not a region is selected."
  (interactive)
  (if (my:region-active)
      (call-interactively #'capitalize-region)
    (call-interactively #'capitalize-word)))

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
