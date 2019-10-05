;;; init-process.el --- Running subprocesses -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-shlex)


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


(defmacro my:with-temp-file (file &rest body)
  "Run BODY forms with temporaty file.
File is removed afterwards. Filename is binded to symbol name FILE."
  (declare (indent 1) (debug t))
  `(let ((,file (make-temp-file
                 (expand-file-name "scor"
                                   (or small-temporary-file-directory
                                       temporary-file-directory)))))
     (unwind-protect
         ,@body
       (file-exists-p ,file))))


(defun -my:process-error-display (error-file error-buffer display)
  "Display ERROR-FILE contents in ERROR-BUFFER.
Raise buffer if DISPLAY is not nil."
  (when (and error-file (file-exists-p error-file))
    (if (< 0 (nth 7 (file-attributes error-file)))
        (with-current-buffer (get-buffer-create error-buffer)
          (let ((pos-from-end (- (point-max) (point))))
            (or (bobp) (insert "\f\n"))
            ;; Do no formatting while reading error file,
            ;; because that can run a shell command, and we
            ;; don't want that to cause an infinite recursion.
            (format-insert-file error-file nil)
            ;; Put point after the inserted errors.
            (goto-char (- (point-max) pos-from-end)))
          (and display (display-buffer (current-buffer)))))))


(defun -my:process-exit-status-string (exit-status)
  "Format text description for EXIT-STATUS of shell command."
  (cond ((null exit-status)
         "(Shell command failed with error)")
        ((equal 0 exit-status)
         "(Shell command succeeded)")
        ((stringp exit-status)
         (format "(Shell command killed by signal %s)" exit-status))
        (t
         (format "(Shell command failed with code %d)" exit-status))))


(defun -my:process-region-noncontigous (start command args)
  "Execute COMMAND with ARGS lisk against current non-contigous region.
Point will be at START position. See `region-noncontiguous-p'."
  (let (exit-status)
    (my:with-temp-file error-file
      (let ((input (concat (funcall region-extract-function 'delete) "\n"))
            output)
        (with-temp-buffer
          (insert input)
          (setq exit-status
                (apply #'call-process-region (point-min) (point-max)
                       command
                       t (list t error-file) nil
                       args))
          (setq output (split-string (buffer-string) "\n")))
        (goto-char start)
        (funcall region-insert-function output))
      (-my:process-error-display
       error-file shell-command-default-error-buffer t))
    exit-status))


(defun -my:process-region (start end command args &optional keep)
  "Execute COMMAND with ARGS over active region or entire buffer.
Text between START and END positions will be passed to the process and
then replaced by output."
  (let* ((swap (< start end)) exit-status)
    (my:with-temp-file error-file
      (goto-char start)
      (push-mark (point) 'nomsg)
      (setq exit-status
            (apply #'call-process-region start end
                   command
                   (not keep)
                   (list t error-file)
                   nil
                   args))
      (and swap (exchange-point-and-mark))
      (-my:process-error-display
       error-file shell-command-default-error-buffer t))
    exit-status))


(defun my:process-region (start end command &rest args)
  "Execute COMMAND with ARGS over active region or entire buffer.
Text between START and END positions will be passed to the process and
then replaced by output."
  (interactive (let* ((args (my:sh-tokenize
                             (read-shell-command "Shell command on region: ")))
                      (bounds (if (region-active-p)
                                  (cons (region-beginning) (region-end))
                                (cons (point-min) (point-max)))))
          (append
           (list (car bounds) (cdr bounds) (car args))
           (cdr args))))
  (message
   (-my:process-exit-status-string
    (if (region-noncontiguous-p)
        (-my:process-region-noncontigous start command args)
      (-my:process-region start end command args)))))


(provide 'init-process)

;;; init-process.el ends here
