;;; init-edit.el --- Editing routines -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq-default
 ;; No-op for region operations if region is active but empty
 ;; useful for macros consistency
 use-empty-active-region t
 shift-select-mode nil
 set-mark-command-repeat-pop t
 sentence-end-double-space nil
 require-final-newline t
 indent-tabs-mode nil)

(electric-pair-mode t)


(defun my:push-mark-no-activate ()
  "Call `push-mark' like `push-mark-command' but without activation."
  (interactive)
  (push-mark))

;; Do not activate mark during jump
(defun my:exchange-point-and-mark (&optional arg)
  "Inverses prefix ARG of `exchange-point-and-mark'when mark is not active (see `mark-active')."
  (interactive "P")
  (exchange-point-and-mark
   (if mark-active arg (not arg))))

(defun my:kill-line-to-indent ()
  "Kill line backward (opposite to `kill-line') and indent after that."
  (interactive "*")
  (kill-line 0)
  (indent-according-to-mode))

;; Region dependent choices
(defun my:kill-region-or-word (arg)
  "Call `kill-region' or `backward-kill-word' when regin is active or not.
ARG sets number of words to kill backward when region is not active."
  (interactive "*p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun my:delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "*p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my:backward-delete-word (arg)
  "Delete word backward until encountering end of previous word.
With argument ARG, do this many times."
  (interactive "*p")
  (my:delete-word (- arg)))

(defun my:join-line (arg)
  "Backward from `delete-indentation'.
Joins this line to following line.
With ARG join this line to previous line"
  (interactive "*p")
  (let ((join? (if (>= arg 0) t nil))
        (counter (abs arg)))
    (while (> counter 0)
      (delete-indentation join?)
      (setq counter (1- counter)))))

(defun my:open-line (arg)
  "Move to the next line and then opens a line. Do ARG times.
See also `newline-and-indent'."
  (interactive "*p")
  (cond ((> arg 0)
         (end-of-line)
         (newline arg 'interactive))
        ((< arg 0)
         (beginning-of-line)
         (newline (abs arg) 'interactive)
         (forward-line -1)
         (indent-according-to-mode))))

(defun my:forward-same-syntax (arg)
  "Same as `forward-same-syntax', but ignore errors. Do ARG times."
  (interactive "^p")
  (ignore-errors (forward-same-syntax arg)))

(defun my:backward-same-syntax (arg)
  "Same as `forward-same-syntax', ignore errors and invert ARG.
See `my:forward-same-syntax'"
  (interactive "^p")
  (ignore-errors (forward-same-syntax (- arg))))

(defun my:open-line-back (arg)
  "Open a new line before the current one. Do ARG times.
See also `my:open-line' and `newline-and-indent'."
  (interactive "*p")
  (my:open-line (- arg)))

(defun my:move-line-up ()
  "Move up the current line."
  (interactive "*")
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun my:move-line-down ()
  "Move down the current line."
  (interactive "*")
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun my:minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then take a second `keyboard-quit' to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (deactivate-mark)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my:kill-buffer ()
  "Kill current active buffer without prompt."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my:kill-buffer-and-window ()
  "Kill current active buffer without prompt then close current window."
  (interactive)
  (let ((buf (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (kill-buffer buf)))

(defun my:keep-only-region (start end)
  "Remove everything before and after the region.
START and END positions define the region to keep."
  (interactive
   (list (region-beginning)
         (region-end)))
  (delete-region (max start end) (point-max))
  (delete-region (point-min) (min start end)))


(provide 'init-edit)

;;; init-edit.el ends here
