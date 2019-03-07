;;; init-flymake.el --- Flymake customization  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code

(require 'flymake)


(defun -my:flymake--highligh-line-if-valid-range (diagnostic)
  "Fix disabling the backend if region range is (nil . nil).
Set diagnostic region as last character of the buffer. DIAGNOSTIC is
`flymake--diag' struct."
  (when (not (and (flymake--diag-beg diagnostic)
                  (flymake--diag-end diagnostic)))
    (let* ((buffer (flymake--diag-buffer diagnostic))
           (end (with-current-buffer buffer (point-max)))
           (begin (max (1- end) 1)))
      (setf (flymake--diag-beg diagnostic) begin)
      (setf (flymake--diag-end diagnostic) end))))


(advice-add #'flymake--highlight-line :before
            #'-my:flymake--highligh-line-if-valid-range)


(provide 'init-flymake)

;;; init-flymake.el ends here
